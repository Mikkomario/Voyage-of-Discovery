package vf.voyage.controller

import utopia.flow.collection.immutable.Pair
import utopia.flow.util.console.ConsoleExtensions._
import vf.voyage.controller.action.{CreateCharacter, LlmActions}
import vf.voyage.model.context.{Gf, Player}
import vf.voyage.model.enumeration.Gender.{Female, Male}

import scala.io.StdIn

/**
 * The main application object for this project
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
object VoyageOfDiscoveryApp extends App
{
	println("Welcome to Voyage of Discovery - a role-playing game")
	
	// Starts by setting up the game facilitator
	println("\nWhich model should I use for facilitating this game?")
	LlmActions.selectModel().flatMap { llm =>
		println(s"Okay. I will use $llm")
		val gfName = StdIn.readNonEmptyLine("How do you want to call me?").getOrElse("GF")
		val playerName = StdIn.readNonEmptyLine(s"Ok ;). How do you want me to call you?").getOrElse("player")
		println(s"$playerName, what a nice name. And your preferred pronoun?")
		val playerGender = StdIn.selectFrom(Pair(Male -> "He (man)", Female -> "She (woman)"), "pronouns").getOrElse {
			println("I understand. I will just pick the statistical average, then.")
			Male
		}
		implicit val gf: Gf = Gf(llm, gfName, Player(playerName, playerGender))
			
		println(s"Nice. Now, let's design your character")
		CreateCharacter().map { gf -> _ }
	} match {
		case Some((facilitator, character)) =>
			println("\nTesting:")
			println(s"GF name: ${ facilitator.name }")
			println(s"Character name: ${ character.name }")
			println(s"Character description: \n\"${ character.description }\"")
			
			println("To be continued...")
		
		case None => println("Ok. Let's not play this time. See you later!")
	}
}
