package vf.voyage.controller.action

import utopia.annex.model.response.Response
import utopia.annex.util.RequestResultExtensions._
import utopia.echo.model.LlmDesignator
import utopia.echo.model.response.generate.StreamedReply
import utopia.flow.async.AsyncExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.parse.string.Regex
import utopia.flow.util.console.ConsoleExtensions._
import utopia.flow.util.StringExtensions._
import utopia.flow.view.mutable.caching.ResettableLazy
import utopia.flow.view.mutable.eventful.SettableOnce
import vf.voyage.model.context.{CharacterDescription, Gf}
import vf.voyage.controller.Common._

import scala.collection.immutable.VectorBuilder
import scala.io.StdIn
import scala.util.{Failure, Success}

/**
 * Provides interactive actions for setting up the game (theme, character, stuff like that)
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
object CreateCharacter
{
	// ATTRIBUTES   ------------------------
	
	private val nameWrapperRegex = Regex.escape('$')
	private val nameRegex = nameWrapperRegex + Regex.any + nameWrapperRegex
	
	
	// OTHER    ----------------------------
	
	/**
	 * Interacts with the user in order to come up with a game character
	 * @param gf The game facilitator
	 * @return Description of the player's character. None if character-creation was canceled.
	 */
	def apply()(implicit gf: Gf) = {
		// Starts by writing the character description
		println("How do you want to approach character-creation?")
		StdIn.selectFrom(Pair(1 -> s"Brainstorm with an ${ gf.name }", 2 -> "Write the character yourself"))
			.flatMap {
				// Case: Brainstorming
				case 1 => brainstormCharacterCreation()
				// Case: Write your own
				case _ => StdIn.readNonEmptyLine("Please write a description for your character")
			}
			// Next names the character
			.flatMap { characterDescription =>
				nameCharacter(characterDescription).map { name => CharacterDescription(name, characterDescription) }
			}
	}
	
	private def nameCharacter(characterDescription: String)(implicit gf: Gf) = {
		println("Do you want to name your character right away, or do you want some ideas first?")
		StdIn.selectFrom(Pair(1 -> "I can name them now", 2 -> "Give me some ideas first")).flatMap {
			// Case: Immediate naming
			case 1 => StdIn.readNonEmptyLine("Please name your character")
			// Case: AI-assisted naming
			case _ =>
				println(s"Asking $gf to come up with name-ideas for your character...")
				ollama.generate(s"Here's a description of a role-playing character: \"$characterDescription\"\n\nPlease come up with 8 different names that could fit this character. \nWrap each name between dollar signs (e.g. $$Adam$$ is viable syntax for name \"Adam\")")
					.tryFlatMapSuccess(parseNameIdeas).waitForResult() match
				{
					case Success(nameIdeas) =>
						println()
						StdIn.selectFromOrAdd(nameIdeas.map { n => n -> n }, "names") {
							StdIn.readNonEmptyLine("Please name your character") }
					
					case Failure(error) =>
						log(error, "Failed to generate name ideas")
						None
				}
		}
	}
	
	private def brainstormCharacterCreation()(implicit gf: Gf): Option[String] = {
		println("Please write some words that may relate to your character. \nThese are used for providing the AI some inspiration.")
		StdIn.readNonEmptyLine().flatMap { inspiration =>
			println(s"Asking ${ gf.name } to come up with some character ideas...")
			ollama.generate(s"Come up with 5 different ideas for a role-playing character. Here are some words that may relate to these characters: $inspiration. Start the description of each character with [CHAR].")
				.waitForResult().toTry.flatMap { parseCharacterIdeas(_).waitFor() } match
			{
				case Success(characters) =>
					println("\nWhat do you want to do next?")
					StdIn.selectFrom(Vector(1 -> "Select one of these", 2 -> "Write your own", 3 -> "Try again")).flatMap {
						// Case: Selecting from AI-generated characters
						case 1 =>
							StdIn.selectFrom(characters.map { c => c -> s"${ c.linesIterator.next().take(100) }..." })
						
						// Case: Writing a character
						case 2 => StdIn.readNonEmptyLine("Please write a description for your character")
						
						// Case: Retry
						case _ => brainstormCharacterCreation()
					}
				
				case Failure(error) =>
					log(error, "Response-generation failed")
					None
			}
		}
	}
	
	private def parseCharacterIdeas(reply: StreamedReply) = {
		// Pointers for building the characters while reading text data
		val completedCharsBuilder = new VectorBuilder[String]()
		val currentBuilderPointer = ResettableLazy { new StringBuilder() }
		
		// Listens to text updates and prints them. Also updates character text in real time.
		println()
		reply.newTextPointer.addContinuousListenerAndSimulateEvent("") { e =>
			if (e.newValue.contains("[CHAR]")) {
				val (toOld, toNew) = e.newValue.splitAtFirst("[CHAR]").toTuple
				print(toOld)
				println()
				print(toNew)
				
				// Finishes the old character description
				toOld.ifNotEmpty.foreach { text =>
					currentBuilderPointer.current.foreach { b => b ++= text }
				}
				currentBuilderPointer.popCurrent().foreach { b => completedCharsBuilder += b.result() }
				
				// Starts a new one
				currentBuilderPointer.value ++= toNew
			}
			else {
				print(e.newValue)
				currentBuilderPointer.current.foreach { _ ++= e.newValue }
			}
		}
		
		// Once all text has been read, builds and returns the character vector
		val charactersPointer = SettableOnce[Vector[String]]()
		reply.newTextPointer.addChangingStoppedListenerAndSimulateEvent {
			currentBuilderPointer.popCurrent().foreach { b => completedCharsBuilder += b.result() }
			charactersPointer.set(completedCharsBuilder.result())
		}
		
		charactersPointer.future
	}
	
	private def parseNameIdeas(reply: StreamedReply) = {
		// Prints the reply
		reply.printAsReceived { _.replaceEachMatchOf(nameWrapperRegex, "") }
		// Once read, parses the names from the reply
		reply.future.mapIfSuccess { reply =>
			nameRegex.matchesIteratorFrom(reply.text).map { name => name.drop(1).dropRight(1) }.toVector
		}
	}
}
