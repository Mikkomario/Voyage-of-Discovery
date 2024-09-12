package vf.voyage.controller.action

import utopia.annex.util.RequestResultExtensions._
import utopia.echo.model.enumeration.ChatRole.{Assistant, User}
import utopia.echo.model.request.generate.Prompt
import utopia.echo.model.response.OllamaResponse
import utopia.flow.async.AsyncExtensions._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Empty, Pair}
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.util.NotEmpty
import utopia.flow.util.StringExtensions._
import utopia.flow.util.console.ConsoleExtensions._
import vf.voyage.controller.Common._
import vf.voyage.model.context.{CharacterDescription, GameSetting, Gf}
import vf.voyage.model.enumeration.CompassDirection
import vf.voyage.model.enumeration.GfRole.Designer

import scala.io.StdIn
import scala.collection.mutable
import scala.util.Random

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
		val genrePrompt = s"Assign a suitable genre for this game. The genre should facilitate role-playing and exploration. Only respond with the genre's name, possibly followed by its short description.\n\nFor reference, here's a description of the game's protagonist: ${
			protagonist.description.endingWith(".") }"
		ollama.generate(genrePrompt)
			.tryFlatMapSuccess(printAndReturn)
			.waitForResult().logToOption
			.flatMap { genre =>
				// Generating alternative themes
				println("\n\nNext let's figure out a theme...")
				val themeMessageHistory = Pair(User(genrePrompt), Assistant(genre))
				ollama.chat("Great. Come up with 5 alternative themes for this game. Start each theme with its index and keep each theme on a separate line.",
						themeMessageHistory)
					.tryFlatMapSuccess(parseIndexedList).waitForResult().logToOption
					.flatMap { themes =>
						// Allowing the user to select from the available themes
						val theme = themes.oneOrMany match {
							case Left(only) =>
								println(s"\n\n\nOkay. I finally figured it out. Here's the theme I came up with: $only.\nI Hope you like it.")
								Some(only)
							case Right(themes) =>
								println("\n\nOkay. I came up with a couple themes. Please help me select the one that fits our game best.")
								StdIn.selectFrom(themes.map { t => t -> (t.take(140).untilLast(".") + "...") }, "themes")
						}
						theme.flatMap { theme =>
							// Describing the game world, also
							println("\nOkay. Now we have a theme as well. Let me write a short world description next.")
							ollama.chat("Describe this game's world / environment for me. Where are these events taking place? How is the culture? What kind of geographic environment is it? Are there some important factions culture- and story-wise? Present the description in a concise form that facilitates map design and story-writing. Do not include additional commentary.",
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
	def generateBiome(gf: Gf, neighbouringBiomes: Map[CompassDirection, Pair[String]],
	                  blockedDirections: Set[CompassDirection])
	                 (implicit setting: GameSetting, protagonist: CharacterDescription) =
	{
		implicit val designer: Gf = gf.withRole(Designer)
		val surroundingBiomeDescription = neighbouringBiomes
			.map { case (direction, biomes) =>
				val baseBiomeStr = s" Towards $direction of this area is ${ biomes.first }"
				biomes.second.ifNotEmpty match {
					case Some(furtherBiome) => s"$baseBiomeStr and further $direction is $furtherBiome."
					case None => s"$baseBiomeStr."
				}
			}
			.mkString(". ").appendIfNotEmpty(". ")
		val blockedDirectionsDescription = blockedDirections.emptyOneOrMany match {
			case Some(Left(only)) =>
				s"This area very closed, allowing travel only to/from the $only."
			case Some(Right(many)) =>
				if (many.size == 2)
					s"This area acts as a passageway, allowing travel only to/from ${ many.mkString(" and ") }."
				else
					s"The ${ CompassDirection.values.find { d => !many.contains(d) }.get } side is blocked and can't be traveled to."
			
			case None => "This area is very open, allowing traveling to/from all four cardinal directions."
		}
		
		ollama
			.generateBuffered(Prompt(
				s"Come up with a local environment within this game. These are smaller areas, such as \"a town marketplace\", \"governor's office\", \"a beach\" or \"an entrance to a large cave\".${
					surroundingBiomeDescription.prependIfNotEmpty(" \n") }$blockedDirectionsDescription",
				systemMessage = setting.systemMessage)
				.toQuery)
			.mapSuccess { _.text }
	}
	
	/**
	 * Generates 3 options for the game's starting area and allows the player to choose one they like the best
	 * @param gf The game's facilitator
	 * @param setting Game's setting (implicit)
	 * @param protagonist Game's protagonist (implicit)
	 * @return Area selected by the user, plus a future that resolves into the number of exits to have in this area
	 *         (or to a failure).
	 *         None if no area was selected or if area-generation failed.
	 */
	def generateStartingBiome(gf: Gf)(implicit setting: GameSetting, protagonist: CharacterDescription) = {
		implicit val designer: Gf = gf.withRole(Designer)
		println(s"\nLet's come up with a few possible starting locations for ${ protagonist.name }'s adventure...")
		val options = ollama.generate(Prompt(
			s"Come up with 3 different areas / local environments where this game could start. These are smaller areas, such as \"${
				protagonist.name }'s home\", \"an entrance to an ancient city's remains\" or \"a small camp at the edge of a forest\". Select areas that are memorable and fit the game's general environment well. The selected areas should also facilitate storytelling, making an interesting start for the game and being somehow connected to ${
				protagonist.name }'s story. Place each area on its own line and prefix each with its numeric index.",
			systemMessage = setting.systemMessageIncludingProtagonist))
			.tryFlatMapSuccess(parseIndexedList).waitForResult().getOrElseLog(Empty)
		
		val areaDescription = NotEmpty(options) match {
			// Allows the user to select the starting area, or to write their own
			case Some(options) =>
				println("\n\nWhich one of these do you think we should start at?")
				StdIn.selectFromOrAdd(options.map { a => a -> a }, "areas") {
					StdIn.readNonEmptyLine(
						"Okay. Go ahead and tell me the starting area and we'll go with that",
						"We can't start the game if we don't choose the starting area. I give you one more chance to write one, if you don't mind.")
				}
			
			// Case: Area-parsing failed => Allows the user to write the area instead
			case None =>
				println("\nLooks like I'm experiencing some technical difficulties... Would you be so kind as to describe the starting area for me?")
				StdIn.readNonEmptyLine(retryPrompt = "Please note that we can't start the game without knowing where to start it from. Could you please describe the starting area, if it's not too much to ask?")
		}
		
		// Also determines the number of exits
		areaDescription.map { description =>
			val exitCountFuture = ollama.generate(
				"How many exits does it make sense to have in this area? Open areas, such as open fields, should have more exits (e.g. 3 or 4) while closed areas, such as rooms and passageways should have fewer (e.g. 1 or 2). Reply only with the number of exits. The number of exits must be a digit between 1 and 4 (inclusive).")
				.tryFlatMapSuccess(extractInt)
			description -> exitCountFuture
		}
	}
	
	/**
	 * Generates the 1-4 areas surrounding the game's start location
	 * @param startingBiome The biome of the starting area
	 * @param startExitCount Number of exits leaving the starting area
	 * @param gf Implicit game facilitator
	 * @param setting Implicit game setting
	 * @param protagonist Implicit game's protagonist
	 * @return Information of the surrounding areas as a Vector.
	 *         Each entry contains the following 3 elements:
	 *              1. This area's direction from the starting area
	 *              1. This area's biome
	 *              1. Directions that may be traversed from this area
	 *
	 *         The resulting Vector contains 1-4 items.
	 *         Yields a failure if the generation process failed. May yield partial failures, also.
	 */
	def generateStartingMap(startingBiome: String, startExitCount: Int)
	                       (implicit gf: Gf, setting: GameSetting, protagonist: CharacterDescription) =
	{
		val unusedDirectionsIter = Random.shuffle(CompassDirection.values).iterator
		val generatedBiomesMap = mutable.Map[CompassDirection, String]()
		// Generates X surrounding biomes
		Iterator.iterate(1) { _ + 1 }.take(startExitCount)
			.map { i =>
				println(s"${ startExitCount - i + 1 } surrounding area(s) remaining...")
				// Determines the accessible travel directions (1-4)
				val directionFromStart = unusedDirectionsIter.next()
				val returnDirection = directionFromStart.opposite
				val otherAccessibleDirections = CompassDirection.randomized.iterator
					.filterNot { _ == returnDirection }.take(1 + Random.nextInt(3)).toSet
				val accessibleDirections = otherAccessibleDirections + returnDirection
				
				// Generates the biome
				generateBiome(gf,
					Map(directionFromStart.opposite ->
						Pair(startingBiome, generatedBiomesMap.getOrElse(returnDirection, ""))),
					CompassDirection.valueSet -- accessibleDirections)
					.waitForTry()
					.map { biome =>
						// Records the result,
						// so that it can be taken into account when generating possible opposite biome
						generatedBiomesMap(directionFromStart) = biome
						(directionFromStart, biome, accessibleDirections)
					}
			}
			.toTryCatch
	}
	
	private def parseIndexedList(reply: OllamaResponse) = {
		reply.printAsReceived()
		reply.future
			.mapIfSuccess {
				_.text.linesIterator.filter { _.take(10).exists { _.isDigit } }
					.map { line => line.drop(line.indexWhere { _.isDigit }).dropWhile { c => !c.isLetter } }
					.toVector
			}
	}
	
	private def extractInt(reply: OllamaResponse) =
		reply.future.tryMapIfSuccess { _.text.dropWhile { !_.isDigit }.takeWhile { _.isDigit }.tryInt }
	
	private def printAndReturn(reply: OllamaResponse) = {
		reply.printAsReceived()
		reply.future.mapIfSuccess { _.text }
	}
}
