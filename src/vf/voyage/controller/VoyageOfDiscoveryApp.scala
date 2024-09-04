package vf.voyage.controller

import utopia.flow.util.console.ConsoleExtensions._
import vf.voyage.controller.action.{CreateCharacter, LlmActions}
import vf.voyage.model.context.Gf

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
	println("Let's start by choosing the game facilitator")
	LlmActions.selectModel().flatMap { llm =>
		implicit val gf: Gf =
			Gf(llm, StdIn.readNonEmptyLine("Please give the game facilitator a name").getOrElse("game facilitator"))
			
		println("\nThank you :)\nNow, let's design your character")
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
