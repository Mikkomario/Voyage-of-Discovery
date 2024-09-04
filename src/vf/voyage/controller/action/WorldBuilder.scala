package vf.voyage.controller.action

import utopia.annex.util.RequestResultExtensions._
import utopia.echo.model.response.generate.StreamedReply
import utopia.flow.async.AsyncExtensions._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.string.Regex
import utopia.flow.util.console.ConsoleExtensions._
import utopia.flow.util.StringExtensions._
import vf.voyage.model.context.{CharacterDescription, Gf}
import vf.voyage.model.enumeration.GfRole.Designer
import vf.voyage.controller.Common._

import scala.io.StdIn
import scala.util.{Failure, Success}

/**
 * Interface for the world-building process (i.e. for the map & theme development)
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
object WorldBuilder
{
	// ATTRIBUTES   -----------------------------
	
	private val braceRegex = Regex.anyOf("{}")
	private val closingBraceRegex = Regex.escape('}')
	private val withinBracesRegex =
		Regex.escape('{') + (!closingBraceRegex).withinParenthesis.anyTimes + closingBraceRegex
	
	
	// OTHER    ---------------------------------
	
	/**
	 * Comes up with a theme for the game
	 * @param gf The game facilitator
	 * @param character The player's character
	 * @return Theme chosen for the game. None if no theme was chosen or if the process failed.
	 */
	def designGameTheme(gf: Gf, character: CharacterDescription) = {
		implicit val designerGf: Gf = gf.withRole(Designer)
		
		println("Please wait a moment while I come up with a theme for our game...")
		// FIXME: The AI wraps the responses in normal parentheses instead of curly brackets
		ollama.generate(s"Here's a description of the game's protagonist: ${
			character.description }\n\nCome up with 4 different themes for a role-playing game featuring this character. Wrap the description of each theme in curly brackets so that my computer application can automatically process them.")
			.tryFlatMapSuccess(parseThemeIdeas).waitForResult()
			.flatMap { _.notEmpty.toTry { new IllegalArgumentException("Theme-generation didn't yield a single result") } } match
		{
			case Success(themes) =>
				themes.oneOrMany match {
					case Left(only) =>
						println(s"\n\nOkay. I finally figured it out. Here's the theme I came up with: $only.\nI Hope you like it.")
						Some(only)
					case Right(themes) =>
						println("\nOkay. I came up with a couple themes. Please help me select the one that fits our game best.")
						StdIn.selectFrom(themes.map { t => t -> t.take(140).untilLast(".") }, "themes")
				}
			
			case Failure(error) =>
				log(error, "Failed to generate a theme")
				None
		}
	}
	
	private def parseThemeIdeas(reply: StreamedReply) = {
		reply.printAsReceived { _.replaceEachMatchOf(braceRegex, "") }
		reply.future
			.mapIfSuccess { _.text.splitIterator(withinBracesRegex)
				.map { s => s.drop(1).dropRight(1) }.filterNot{ _.isEmpty }.toVector }
	}
}
