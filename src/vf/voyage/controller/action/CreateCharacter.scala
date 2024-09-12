package vf.voyage.controller.action

import utopia.annex.util.RequestResultExtensions._
import utopia.echo.model.ChatMessage
import utopia.echo.model.enumeration.ChatRole.{Assistant, System, User}
import utopia.echo.model.enumeration.ModelParameter.PredictTokens
import utopia.echo.model.request.generate.Prompt
import utopia.echo.model.response.OllamaResponse
import utopia.echo.model.response.chat.StreamedReplyMessage
import utopia.flow.async.AsyncExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.util.StringExtensions._
import utopia.flow.util.console.ConsoleExtensions._
import vf.voyage.controller.Common._
import vf.voyage.model.context.{CharacterDescription, Gf}
import vf.voyage.model.enumeration.Gender
import vf.voyage.model.enumeration.Gender.{Female, Male, Undefined}
import vf.voyage.model.enumeration.GfRole.Designer

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

/**
 * Provides interactive actions for setting up the game (theme, character, stuff like that)
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
object CreateCharacter
{
	// ATTRIBUTES   ------------------------
	
	// Uses an empty character description in character-creation requests
	private implicit val placeholderChar: CharacterDescription = CharacterDescription.empty
	
	
	// OTHER    ----------------------------
	
	/**
	 * Interacts with the user in order to come up with a game character
	 * @param gf The game facilitator
	 * @return Description of the player's character. None if character-creation was canceled or if it failed.
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
				nameCharacter(characterDescription)
					.map { name =>
						// Finally identifies the character's gender
						identifyCharacterGender(gf, name, characterDescription).waitForResult() match {
							case Success((gender, newDescription)) =>
								CharacterDescription(name, newDescription, gender)
							case Failure(error) =>
								log(error, "Failed to identify character's gender")
								CharacterDescription(name, characterDescription, Undefined)
						}
					}
			}
	}
	
	// TODO: Remove empty lines or penalize newline
	private def brainstormCharacterCreation()(implicit gf: Gf): Option[String] = {
		println(s"Give me some inspiration by writing some words that may relate to your character.")
		StdIn.readNonEmptyLine().flatMap { inspiration =>
			println(s"\nInspiring! Let's see what I can come up with...")
			ollama.generate(s"Come up with 5 different ideas for my role-playing character. Here are some words that may relate to these characters: $inspiration. Keep each description on a single line and prefix each with its index. Also, don't name the character at this point. I will name them later.")
				.tryFlatMapSuccess(ollama.parseIndexedList).waitForResult() match
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
												User(s"Come up an idea for a role-playing character. Here are some words that may relate to this character: $inspiration. Do not name the character at this point, however."),
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
			ollama.chat(
					s"This description is good! However, please write another version which incorporates these changes: $change",
					previousConversation)
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
	
	private def nameCharacter(characterDescription: String)(implicit gf: Gf) = {
		println("\nDo you want to name your character right away, or do you want some ideas first?")
		StdIn.selectFrom(Pair(1 -> "I can name them now", 2 -> "Give me some ideas first")).flatMap {
			// Case: Immediate naming
			case 1 => StdIn.readNonEmptyLine("Please name your character")
			// Case: AI-assisted naming
			case _ =>
				println(s"Let's see...")
				ollama.generate(s"Help me come up with a name for my character. Come up with at least 8 different names that go well with my character's description. Place each name on a separate line, prefixing it with its index. \nHere's the description of my character: $characterDescription")
					.tryFlatMapSuccess(ollama.parseIndexedList).waitForResult() match
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
	
	/**
	 * Identifies whether the character is a male or a female.
	 * Also updates the character description, if appropriate.
	 * May interact with the user in order to come to a conclusion.
	 * @param gf The game facilitator
	 * @param name Character's name
	 * @param description Character's description
	 * @return Future that resolves into the character's gender and the new version of character description.
	 *         Yields a failure if failed to converse with the LLM about this.
	 */
	private def identifyCharacterGender(gf: Gf, name: String, description: String): Future[Try[(Gender, String)]] = {
		implicit val assistant: Gf = gf.withRole(Designer)
		println(s"\nExcellent. Let's make sure that I got $name's gender correct...")
		// Checks whether the character description refers to a specific gender
		val responseInstruction = "Respond only with the word \"male\", \"female\" or \"neutral\""
		val descriptionGenderPrompt = s"Does the following character description refer to a male or a female, or is it gender-neutral? $responseInstruction. \nCharacter description: \"$description\""
		ollama.generateBuffered(
				Prompt(descriptionGenderPrompt).toQuery,
				Map(PredictTokens -> 4))
			.flatMap { descriptionGenderReply =>
				descriptionGenderReply.toTry.map(parseGenderFrom).map { descriptionGender =>
					// Checks whether the character's name is gender-specific
					val descriptionGenderStr = descriptionGender.binary match {
						case Some(gender) => gender.toString
						case None => "neutral"
					}
					// TODO: Don't include the previous prompt here
					val nameGenderPrompt = s"Is the name \"$name\" masculine, feminine or gender-neutral? $responseInstruction."
					val nameGenderMessageHistory = Pair(User(descriptionGenderPrompt), Assistant(descriptionGenderStr))
					ollama.chatBuffered(nameGenderPrompt, nameGenderMessageHistory, Map(PredictTokens -> 4))
						.flatMapSuccessToTry { nameGenderReply =>
							val nameGender = parseGenderFrom(nameGenderReply)
							val genderDefinitions = Pair(nameGender, descriptionGender).map { _.binary }
							
							genderDefinitions.merge { (name, desc) => name.filter(desc.contains) } match {
								// Case: The gender is specified in both the description and the name
								//       => Assumes that its correct
								case Some(clearGender) =>
									println(s"Looks like $name is $clearGender")
									Future.successful(clearGender -> description)
								
								// Case: The gender is not specified in both the description and the name
								//       => Checks which gender to assume, if any,
								//          and checks whether that assumption is correct
								case None =>
									// 'characterGender' will be None only if the user refused to specify it
									val characterGender = genderDefinitions
										.merge { (name, desc) =>
											name match {
												case Some(nameGender) =>
													if (desc.forall { _ == nameGender }) Some(nameGender) else None
												case None => desc
											}
										} match
									{
										// Case: Gender may be assumed based on name or description
										//       => Makes sure the assumption is correct
										case Some(assumedGender) =>
											if (StdIn.ask(s"Am I correct to assume that $name is $assumedGender?",
												default = true))
												assumedGender
											else
												assumedGender.opposite
										
										// Case: Both the description and the name are gender-neutral
										//       => Asks the user to specify the gender
										case None =>
											println(s"I'm not sure about $name's gender yet. Which one should I go with?")
											StdIn.selectFrom(Gender.binaryValues.map { g => g -> g.toString.capitalize },
												"genders")
												.getOrElse(Undefined)
									}
									
									// Updates the character description, if appropriate
									val updatedDescriptionFuture = characterGender.binary
										.filterNot { _ == descriptionGender } match
									{
										case Some(gender) =>
											println(s"Let me update $name's description to reflect ${
												gender.pronounPossessive
											} gender...")
											val updateDescriptionPrompt = descriptionGender.binary match {
												case Some(earlierGender) =>
													s"Please write a new version of the aforementioned character description, replacing all ${
														earlierGender.masculinity
													} references with ${
														gender.masculinity
													} ones (${earlierGender.pronoun} with ${
														gender.pronoun
													}, ${earlierGender.pronounPossessive} with ${
														gender.pronounPossessive
													}, etc.)"
												case None =>
													s"Please write a new version of the aforementioned character description, replacing all gender-neutral references such as \"they\" or \"he or she\" with ${
														gender.masculinity
													} references (${gender.pronoun}, ${gender.pronounPossessive}, etc.)"
											}
											ollama.chat(updateDescriptionPrompt,
													nameGenderMessageHistory ++
														Pair(User(nameGenderPrompt), nameGenderReply.message))
												.tryFlatMapSuccess(printAndReturn)
												.map {
													case Success(newDescription) =>
														if (StdIn.ask(
															"\nIs it okay for me to use this description instead of the previous version?",
															default = true)) {
															println("Noted!")
															newDescription
																.dropWhile { _ == '"' }.dropRightWhile { _ == '"' }
														}
														else {
															println("Okay. I will continue to use the previous version instead.")
															description
														}
													
													case Failure(error) =>
														log(error, "Failed to write the new description version")
														println(s"Unfortunately something went wrong while editing $name's description. Let's continue with the previous version instead.")
														description
												}
										
										case None => Future.successful(description)
									}
									
									// Returns the final gender and the updated description
									updatedDescriptionFuture.map { updatedDescription =>
										characterGender -> updatedDescription
									}
							}
						}
				}.flattenToFuture
			}
	}
	
	private def parseEditedCharacterDescription(reply: StreamedReplyMessage) = {
		reply.printAsReceived()
		reply.future.mapIfSuccess { _.text }
	}
	
	// NB: Assumes that the reply is buffered
	private def parseGenderFrom(reply: OllamaResponse): Gender = {
		if (reply.text.containsIgnoreCase("female"))
			Female
		else if (reply.text.containsIgnoreCase("male"))
			Male
		else
			Undefined
	}
	
	private def printAndReturn(reply: OllamaResponse) = {
		reply.printAsReceived()
		reply.future.mapIfSuccess { _.text }
	}
}
