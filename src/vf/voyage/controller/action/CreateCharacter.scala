package vf.voyage.controller.action

import utopia.annex.util.RequestResultExtensions._
import utopia.echo.model.ChatMessage
import utopia.echo.model.enumeration.ChatRole.{Assistant, System, User}
import utopia.echo.model.response.chat.StreamedReplyMessage
import utopia.echo.model.response.generate.StreamedReply
import utopia.flow.async.AsyncExtensions._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Empty, Pair}
import utopia.flow.collection.mutable.iterator.OptionsIterator
import utopia.flow.parse.string.Regex
import utopia.flow.util.StringExtensions._
import utopia.flow.util.console.ConsoleExtensions._
import vf.voyage.controller.Common._
import vf.voyage.model.context.{CharacterDescription, Gf}

import scala.annotation.tailrec
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
	
	private val newCharacterIndicator = "CHAR:"
	private val newCharacterRegex = Regex("CHAR") + Regex.escape(':')
	
	private val nameWrapperRegex = Regex.escape('$')
	private val nameRegex = nameWrapperRegex + !nameWrapperRegex + Regex.any + nameWrapperRegex
	
	
	// OTHER    ----------------------------
	
	/**
	 * Interacts with the user in order to come up with a game character
	 * @param gf The game facilitator
	 * @return Description of the player's character. None if character-creation was canceled.
	 */
	def apply()(implicit gf: Gf) = {
		// Starts by writing the character description
		println("How do you want us to approach character creation?")
		StdIn.selectFrom(Pair(1 -> s"Let's brainstorm together", 2 -> "I want to write my character myself"), "options")
			.flatMap {
				// Case: Brainstorming
				case 1 => brainstormCharacterCreation()
				// Case: Write your own
				case _ => StdIn.readNonEmptyLine("Okay. Please write the description for your character")
			}
			// Next names the character
			.flatMap { characterDescription =>
				nameCharacter(characterDescription).map { name => CharacterDescription(name, characterDescription) }
			}
	}
	
	private def nameCharacter(characterDescription: String)(implicit gf: Gf) = {
		println("\nDo you want to name your character right away, or do you want some ideas first?")
		StdIn.selectFrom(Pair(1 -> "I can name them now", 2 -> "Give me some ideas first")).flatMap {
			// Case: Immediate naming
			case 1 => StdIn.readNonEmptyLine("Please name your character")
			// Case: AI-assisted naming
			case _ =>
				println(s"Let's see...")
				ollama.generate(s"Help me come up with a name for my character. Here's the description of my character: \"$characterDescription\"\nCome up with at least 8 different names that go well with my character's description. \nWrap each name between dollar signs (e.g. $$Adam$$ is viable syntax for name Adam)")
					.tryFlatMapSuccess(parseNameIdeas).waitForResult() match
				{
					case Success(nameIdeas) =>
						// TODO: Add a familiar name once it has a purpose in the game
						if (nameIdeas.isEmpty)
							StdIn.readNonEmptyLine("\nOkay. Write the name you like the best. And feel free to create a new one if you want to.")
						else {
							println("\nI hope you like these ideas. Feel free to come up with your own as well.")
							StdIn.selectFromOrAdd(nameIdeas.map { n => n -> n }, "names") {
								StdIn.readNonEmptyLine("Please name your character") }
						}
					
					case Failure(error) =>
						log(error, "Failed to generate name ideas")
						None
				}
		}
	}
	
	private def brainstormCharacterCreation()(implicit gf: Gf): Option[String] = {
		println(s"Give me some inspiration by writing some words that may relate to your character.")
		StdIn.readNonEmptyLine().flatMap { inspiration =>
			println(s"\nInspiring! Let's see what I can come up with...")
			ollama.generate(s"Come up with 5 different ideas for my role-playing character. Here are some words that may relate to these characters: $inspiration. Start the description of each character with \"$newCharacterIndicator\". Also, don't name the character at this point. I will name them later.")
				.tryFlatMapSuccess(parseCharacterIdeas).waitForResult() match
			{
				case Success(characters) =>
					println("\n\nHow do you like these? We can always edit the one you choose, if you want.")
					StdIn.selectFrom(Vector(
							1 -> "Nice. I will pick one of these.",
							2 -> "I think I will rather write my own character",
							3 -> "Let's try again"))
						.flatMap {
							// Case: Selecting from AI-generated characters
							case 1 =>
								println("Okay. Which character would you like to continue with?")
								StdIn.selectFrom(characters
										.map { c => c -> s"${ c.linesIterator.next().take(140).untilLast(".") }..." })
									.map { description =>
										if (StdIn.ask(s"Great choice :). Do you want to make some changes to this character's description?")) {
											println("Okay. Here's the current description for reference:")
											println(description)
											println()
											editCharacterDescription(description, Vector(
												System(s"${ gf.player } and you are designing a role-playing character for ${
													gf.player.gender.pronounObject }"),
												User(s"Come up an idea for a role-playing character. Here are some words that may relate to this character: $inspiration."),
												Assistant(description)))
										}
										else
											description
									}
							
							// Case: Writing a character
							case 2 => StdIn.readNonEmptyLine("Makes sense. Please write a description for your character")
							
							// Case: Retry
							case _ =>
								println("\nOkay. Let's try again.")
								brainstormCharacterCreation()
						}
				
				case Failure(error) =>
					log(error, "Response-generation failed")
					None
			}
		}
	}
	
	@tailrec
	private def editCharacterDescription(description: String, previousConversation: Seq[ChatMessage])
	                                     (implicit gf: Gf): String =
	{
		println(s"What changes do you want to make to this character?")
		val change = StdIn.readLine()
		
		// Case: No changes specified => Allows retry, if the user wants to
		if (change.isEmpty) {
			if (StdIn.ask("Do you want to continue without making any changes to your character?", default = true))
				description
			else
				editCharacterDescription(description, previousConversation)
		}
		else {
			// Applies the changes
			println(s"\nOk. Let's see if I can apply these changes...")
			ollama.chat(s"Please write a new version which incorporates this change: $change", previousConversation)
				.tryFlatMapSuccess(parseEditedCharacterDescription)
				.waitForResult() match
			{
				case Success(newDescription) =>
					// Asks the user on how to proceed
					println("\n\nAre you happy with this version, or do you want to make more changes?")
					// NB: Once we use a more sophisticated UI, allow the user to edit the response themselves
					StdIn.selectFrom(Vector(
						1 -> "This looks good",
						2 -> "Let's make some more changes",
						3 -> "I don't like this change. Let's try again.",
						4 -> "The previous version was better. Let's use that one instead.",
						5 -> "That's enough source material. I will write the final version myself.")).getOrElse(1) match
					{
						// Case: More edits
						case 2 =>
							editCharacterDescription(newDescription,
								previousConversation ++ Pair(User(change), Assistant(newDescription)))
						// Case: Retry
						case 3 => editCharacterDescription(description, previousConversation)
						// Case: Continue with the original version
						case 4 => description
						// Case: Manual final edit
						case 5 =>
							println("Sounds good. Please write the final version now.")
							StdIn.readLine()
						// Case: Continue with this edited version
						case _ => newDescription
					}
				
				case Failure(error) =>
					log(error, "Failed to request changes to the character description")
					description
			}
		}
	}
	
	private def parseCharacterIdeas(reply: StreamedReply) = {
		reply.printAsReceived { _.replaceEachMatchOf(newCharacterRegex, "") }
		reply.future.mapIfSuccess { reply =>
			reply.text.afterFirst(newCharacterIndicator).splitIterator(newCharacterRegex)
				.map { raw =>
					val trimmed = raw.trim
					if (trimmed.isMultiLine)
						trimmed.untilLast("\n")
					else
						trimmed
				}
				.toVector
		}
	}
	
	private def parseEditedCharacterDescription(reply: StreamedReplyMessage) = {
		reply.printAsReceived()
		reply.future.mapIfSuccess { _.text }
	}
	
	private def parseNameIdeas(reply: StreamedReply) = {
		// Prints the reply
		reply.printAsReceived { _.replaceEachMatchOf(nameWrapperRegex, "") }
		// Once read, parses the names from the reply
		reply.future.mapIfSuccess { reply =>
			nameRegex.matchesIteratorFrom(reply.text)
				.map { name => name.afterFirst("$").untilFirst("$") }.filterNot { _.isEmpty }.toVector
		}
	}
}
