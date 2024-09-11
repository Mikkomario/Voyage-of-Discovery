package vf.voyage.controller.action

import utopia.annex.util.RequestResultExtensions._
import utopia.echo.model.enumeration.ChatRole.{Assistant, User}
import utopia.echo.model.request.generate.Prompt
import utopia.echo.model.response.OllamaResponse
import utopia.flow.async.AsyncExtensions._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.util.StringExtensions._
import utopia.flow.util.console.ConsoleExtensions._
import vf.voyage.controller.Common._
import vf.voyage.model.context.{CharacterDescription, GameSetting, Gf}
import vf.voyage.model.enumeration.CompassDirection
import vf.voyage.model.enumeration.GfRole.Designer

import scala.io.StdIn

/**
 * Interface for the world-building process (i.e. for the map & theme development)
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
object WorldBuilder
{
	// OTHER    ---------------------------------
	
	/**
	 * Comes up with a theme for the game
	 * @param gf The game facilitator
	 * @param protagonist The game's protagonist
	 * @return Setting of the game. None if the process failed or was canceled by the user.
	 */
	def designGameSetting(gf: Gf)(implicit protagonist: CharacterDescription) = {
		implicit val designerGf: Gf = gf.withRole(Designer)
		// Identifying the game's genre
		println("Let's come up with the genre first...")
		val genrePrompt = s"Here's a description of the game's protagonist: ${
			protagonist.description }. Assign a suitable genre for this game. The genre should facilitate role-playing and exploration."
		ollama.generate(genrePrompt)
			.tryFlatMapSuccess(printAndReturn)
			.waitForResult().logToOption
			.flatMap { genre =>
				// Generating alternative themes
				println("\n\nNext let's figure out a theme...")
				val themeMessageHistory = Pair(User(genrePrompt), Assistant(genre))
				ollama.chat("Come up with 5 alternative themes for this game. Start each theme with its index and keep each theme on a separate line.",
						themeMessageHistory)
					.tryFlatMapSuccess(parseThemeIdeas).waitForResult().logToOption
					.flatMap { themes =>
						// Allowing the user to select from the available themes
						val theme = themes.oneOrMany match {
							case Left(only) =>
								println(s"\n\n\nOkay. I finally figured it out. Here's the theme I came up with: $only.\nI Hope you like it.")
								Some(only)
							case Right(themes) =>
								println("\nOkay. I came up with a couple themes. Please help me select the one that fits our game best.")
								StdIn.selectFrom(themes.map { t => t -> t.take(140).untilLast(".") }, "themes")
						}
						theme.flatMap { theme =>
							// Describing the game world, also
							println("\nOkay. Now we have a theme as well. Let me write a short world description next.")
							ollama.chat("Describe this game's world / environment for me. Where are these events taking place? How is the culture? What kind of geographic environment is it? Are there some important factions culture- and story-wise?",
								themeMessageHistory ++ Pair(User("Come up with a theme for this game"), Assistant(theme)))
								.tryFlatMapSuccess(printAndReturn).waitForResult().logToOption
								.map { worldDescription =>
									GameSetting(genre, theme, worldDescription)
								}
						}
					}
			}
	}
	
	/**
	 * Generates a short description for a single area or biome
	 * @param gf Game facilitator
	 * @param neighbouringBiomes Descriptions of the surrounding areas.
	 *                           The first values are the closer areas while the second values are areas further off.
	 *                           The second value in each pair may be empty.
	 * @param setting Implicit world setting
	 * @return Future that resolves into an area description, if successful
	 */
	def generateBiome(gf: Gf, neighbouringBiomes: Map[CompassDirection, Pair[String]])
	                 (implicit setting: GameSetting, protagonist: CharacterDescription) =
	{
		implicit val designer: Gf = gf.withRole(Designer)
		val surroundingBiomeDescription = neighbouringBiomes
			.map { case (direction, biomes) =>
				val directionStr = direction.toString.toLowerCase
				val baseBiomeStr = s" Towards $directionStr of this area is ${ biomes.first }"
				biomes.second.notEmpty match {
					case Some(furtherBiome) => s"$baseBiomeStr and further $direction is $furtherBiome."
					case None => s"$baseBiomeStr."
				}
			}
			.mkString(". ")
		
		ollama
			.generateBuffered(Prompt(
				s"Come up with a local environment within this game. These are smaller areas, such as \"a town marketplace\", \"governor's office\", \"a beach\" or \"an entrance to a large cave\".${
					surroundingBiomeDescription.prependIfNotEmpty(" \n") }", systemMessage = setting.systemMessage)
				.toQuery)
			.mapSuccess { _.text }
	}
	
	private def parseThemeIdeas(reply: OllamaResponse) = {
		reply.printAsReceived()
		reply.future
			.mapIfSuccess {
				_.text.linesIterator.filter { _.take(10).exists { _.isDigit } }
					.map { line => line.drop(line.indexWhere { _.isDigit }).dropWhile { c => !c.isLetter } }
					.toVector
			}
	}
	
	private def printAndReturn(reply: OllamaResponse) = {
		reply.printAsReceived()
		reply.future.mapIfSuccess { _.text }
	}
}
