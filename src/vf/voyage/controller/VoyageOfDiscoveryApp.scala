package vf.voyage.controller

import utopia.flow.async.AsyncExtensions._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.console.ConsoleExtensions._
import vf.voyage.controller.Common._
import vf.voyage.controller.action.{CreateCharacter, GameplayLoop, LlmActions, WorldBuilder}
import vf.voyage.model.context.{CharacterDescription, GameSetting, Gf, Player}
import vf.voyage.model.enumeration.Gender.{Female, Male, Undefined}
import vf.voyage.model.enumeration.GfRole.Facilitator
import vf.voyage.model.world.WorldMap

import java.nio.file.Path
import scala.io.StdIn
import scala.util.Random

/**
 * The main application object for this project
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
// TODO: In general, make sure to remove ** from the text
object VoyageOfDiscoveryApp extends App
{
	// ATTRIBUTES   ------------------------
	
	private val saveDir: Path = "data/save"
	saveDir.createDirectories().logFailure
	private val gfPath = saveDir/"gf.json"
	private val characterPath = saveDir/"character.json"
	private val settingPath = saveDir/"setting.json"
	private val mapPath = saveDir/"map.json"
	
	
	// APP CODE ----------------------------
	
	println("Welcome to Voyage of Discovery - a role-playing game")
	private val loadEnabled = {
		if (gfPath.exists) {
			if (StdIn.ask("Do you want to continue from where we last left off?"))
				true
			else {
				saveDir.iterateChildren { _.foreach { _.delete() } }
				false
			}
		}
		else
			false
	}
	
	// Starts by setting up the game facilitator
	(if (loadEnabled) Gf.fromPath(gfPath).logToOption.orElse { setupGf() } else setupGf()).foreach { implicit gf =>
		// Next sets up the character
		val preparedCharacter = if (loadEnabled) CharacterDescription.fromPath(characterPath).toOption else None
		preparedCharacter.orElse { setupCharacter() }.foreach { implicit protagonist =>
			// Next sets up the game theme
			val preparedSetting = if (loadEnabled) GameSetting.fromPath(settingPath).toOption else None
			preparedSetting.orElse { setupSetting() }.foreach { implicit setting =>
				val world = setupWorld()
				world.foreach { implicit world =>
					GameplayLoop.run(gf)
					println("\nTo be continued...")
				}
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
			val playerGender = StdIn.selectFrom(Vector(
				Male -> "He (man)", Female -> "She (woman)", Undefined -> "They (I prefer to stay gender-neutral)"),
				"pronouns")
				.getOrElse {
					println("I understand. I will not make statements about your gender.")
					Undefined
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
	
	private def setupSetting()(implicit gf: Gf, protagonist: CharacterDescription) = {
		println("\nOkay. Let's start designing the game next.")
		val setting = WorldBuilder.designGameSetting(gf)
		setting.foreach { settingPath.writeJson(_).logFailure }
		
		setting
	}
	
	private def setupWorld()(implicit gf: Gf, setting: GameSetting, protagonist: CharacterDescription) = {
		val world = WorldBuilder.generateStartingBiome(gf).flatMap { case (startingBiome, startExitCountFuture) =>
			println("Great. It will take me a while to design the surrounding area...")
			val startExitCount = startExitCountFuture.waitForResult().getOrElseLog { 1 + Random.nextInt(4) }
			WorldBuilder.generateStartingMap(startingBiome, startExitCount).logToOption
				.map { surroundingBiomes => new WorldMap(startingBiome, surroundingBiomes) }
		}
		world.foreach { mapPath.writeJson(_).logFailure }
		world
	}
}
