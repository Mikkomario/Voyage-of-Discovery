package vf.voyage.controller

import utopia.flow.async.AsyncExtensions._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.util.console.ConsoleExtensions._
import vf.voyage.controller.Common._
import vf.voyage.controller.action.{CreateCharacter, GameplayLoop, LlmActions, WorldBuilder}
import vf.voyage.model.context.{CharacterDescription, GameSetting, Gf, Player}
import vf.voyage.model.enumeration.Gender.{Female, Male, Undefined}
import vf.voyage.model.enumeration.GfRole.{Designer, Facilitator, Narrator}
import vf.voyage.model.world.WorldMap

import java.nio.file.Path
import scala.io.StdIn
import scala.util.Random

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
	private val designerPath = saveDir/"designer.json"
	private val gfPath = saveDir/"gf.json"
	private val characterPath = saveDir/"character.json"
	private val settingPath = saveDir/"setting.json"
	private val mapPath = saveDir/"map.json"
	
	
	// APP CODE ----------------------------
	
	println("Welcome to Voyage of Discovery - a role-playing game")
	private val loadEnabled = {
		if (designerPath.exists) {
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
	(if (loadEnabled) Gf.fromPath(designerPath).logToOption.orElse { setupDesigner() } else setupDesigner())
		.foreach { implicit designer =>
			// Next sets up the character
			val preparedCharacter = if (loadEnabled) CharacterDescription.fromPath(characterPath).toOption else None
			preparedCharacter.orElse { setupCharacter() }.foreach { implicit protagonist =>
				// Next sets up the game theme
				val preparedSetting = if (loadEnabled) GameSetting.fromPath(settingPath).toOption else None
				preparedSetting.orElse { setupSetting() }.foreach { implicit setting =>
					val world = setupWorld()
					world.foreach { implicit world =>
						setupGf(designer.player).foreach { gf =>
							GameplayLoop.run(gf)
							println("\nTo be continued...")
						}
					}
				}
			}
			println(s"It was fun playing with you, ${ designer.player }")
		}
		println("See you next time!")
	
	
	// OTHER    ----------------------------
	
	private def setupDesigner() = {
		println("\nHello. My name is Mikko, and our task is to design the next game together. I hope you will have good time.")
		LlmActions.selectModel(
			"A smaller model may be better, since it will be faster. I might benefit from an uncensored model, also.")
			.map { llm =>
				println(s"Okay. I will use $llm")
				val playerName = StdIn.readNonEmptyLine(s"Great. How do you want me to call you?").getOrElse("player")
				println(s"$playerName, what a nice name. And your \"preferred pronoun\"?")
				val playerGender = StdIn.selectFrom(Vector(
					Male -> "He (man)", Female -> "She (woman)", Undefined -> "They (I prefer to stay gender-neutral)"),
					"pronouns")
					.getOrElse {
						println("I understand. I will not make statements about your gender.")
						Undefined
					}
				val gf: Gf = Gf(llm, "Mikko", Player(playerName, playerGender), Designer)
				
				// Saves the facilitator
				designerPath.writeJson(gf).logFailure
				
				gf
			}
	}
	
	private def setupGf(player: Player) = {
		println(s"Hello. I will be playing this game with you, $player.")
		val gfName = StdIn.readNonEmptyLine("How do you want to call me?").getOrElse("narrator")
		println("Great :)")
		LlmActions.selectModel("I need a model with tools support. Picking an uncensored model might be useful, also.")
			.map { llm =>
				val gf = Gf(llm, gfName, player, Narrator)
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
		val setting = WorldBuilder.designGameSetting()
		setting.foreach { settingPath.writeJson(_).logFailure }
		
		setting
	}
	
	private def setupWorld()(implicit gf: Gf, setting: GameSetting, protagonist: CharacterDescription) = {
		val world = WorldBuilder.generateStartingBiome().flatMap { case (startingBiome, startExitCountFuture) =>
			println("Great. It will take me a while to design the surrounding area...")
			val startExitCount = startExitCountFuture.waitForResult().getOrElseLog { 1 + Random.nextInt(4) }
			WorldBuilder.generateStartingMap(startingBiome, startExitCount).logToOption
				.map { surroundingBiomes => new WorldMap(startingBiome, surroundingBiomes) }
		}
		world.foreach { mapPath.writeJson(_).logFailure }
		world
	}
}
