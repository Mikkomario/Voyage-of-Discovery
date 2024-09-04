package vf.voyage.controller

import utopia.flow.collection.immutable.Pair
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.console.ConsoleExtensions._
import vf.voyage.controller.action.{CreateCharacter, LlmActions, WorldBuilder}
import vf.voyage.model.context.{CharacterDescription, Gf, Player}
import vf.voyage.model.enumeration.Gender.{Female, Male}
import vf.voyage.model.enumeration.GfRole.Facilitator
import Common._
import utopia.flow.parse.string.StringFrom

import java.nio.file.Path
import scala.io.StdIn

/**
 * The main application object for this project
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
object VoyageOfDiscoveryApp extends App
{
	// ATTRIBUTES   ------------------------
	
	private val saveDir: Path = "data/save"
	saveDir.createDirectories().logFailure
	private val gfPath = saveDir/"gf.json"
	private val characterPath = saveDir/"character.json"
	private val themePath = saveDir/"theme.txt"
	
	
	// APP CODE ----------------------------
	
	println("Welcome to Voyage of Discovery - a role-playing game")
	private val loadEnabled = gfPath.exists && StdIn.ask("Do you want to continue from where we last left off?")
	
	// Starts by setting up the game facilitator
	(if (loadEnabled) Gf.fromPath(gfPath).logToOption.orElse { setupGf() } else setupGf()).foreach { implicit gf =>
		// Next sets up the character
		val preparedCharacter = if (loadEnabled) CharacterDescription.fromPath(characterPath).toOption else None
		preparedCharacter.orElse { setupCharacter() }.foreach { character =>
			// Next sets up the game theme
			val preparedTheme = if (loadEnabled) StringFrom.path(themePath).toOption else None
			preparedTheme.orElse { setupTheme(character) }.foreach { theme =>
				println("\nTo be continued...")
			}
		}
		println(s"It was fun playing with you, ${ gf.player }")
	}
	println("See you next time!")
	
	
	// OTHER    ----------------------------
	
	private def setupGf() = {
		println("\nWhich model should I use for facilitating this game?")
		LlmActions.selectModel().map { llm =>
			println(s"Okay. I will use $llm")
			val gfName = StdIn.readNonEmptyLine("How do you want to call me?").getOrElse("GF")
			val playerName = StdIn.readNonEmptyLine(s"Ok ;). How do you want me to call you?").getOrElse("player")
			println(s"$playerName, what a nice name. And your preferred pronoun?")
			val playerGender = StdIn.selectFrom(Pair(Male -> "He (man)", Female -> "She (woman)"), "pronouns").getOrElse {
				println("I understand. I will just pick the statistical average, then.")
				Male
			}
			implicit val gf: Gf = Gf(llm, gfName, Player(playerName, playerGender), Facilitator)
			
			// Saves the facilitator
			gfPath.writeJson(gf).logFailure
			
			gf
		}
	}
	
	private def setupCharacter()(implicit gf: Gf) = {
		println(s"Nice. Now, let's design your character")
		val character = CreateCharacter()
		character.foreach { c => characterPath.writeJson(c).logFailure }
		
		character
	}
	
	private def setupTheme(character: CharacterDescription)(implicit gf: Gf) = {
		println("\nOkay. Let's start designing the game next.")
		val theme = WorldBuilder.designGameTheme(gf, character)
		
		theme.foreach { themePath.write(_).logFailure }
		
		theme
	}
}
