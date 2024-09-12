package vf.voyage.controller.action

import utopia.annex.util.RequestResultExtensions._
import utopia.echo.model.enumeration.ChatRole.System
import utopia.flow.async.AsyncExtensions._
import utopia.flow.collection.CollectionExtensions._
import utopia.echo.model.response.chat.ReplyMessage
import utopia.flow.collection.immutable.Single
import vf.voyage.model.context.{CharacterDescription, GameSetting, Gf}
import vf.voyage.model.world.WorldMap
import vf.voyage.controller.Common._
import vf.voyage.model.enumeration.GfRole.Narrator

/**
 * This object runs the main gameplay loop consisting of the narrator's stories, followed by user decisions.
 *
 * @author Mikko Hilpinen
 * @since 11.09.2024, v0.1
 */
object GameplayLoop
{
	def run(gf: Gf)(implicit setting: GameSetting, protagonist: CharacterDescription, world: WorldMap) = {
		implicit val narrator: Gf = gf.withRole(Narrator)
		
		// Starts the game by narrating a short backstory
		tellBackstory().waitForResult().logToOption.foreach { startMessage =>
			// Enters the main loop
			// TODO: Implement
		}
	}
	
	private def tellBackstory()(implicit gf: Gf, setting: GameSetting, protagonist: CharacterDescription, world: WorldMap) =
	{
		val location = world.currentLocation
		ollama.chat(s"${ protagonist.name }'s story starts at ${
			location.biome }. Write a description of this area for the player. Also include a brief backstory on how ${
			protagonist.name } got here. Include an explanation of ${
			protagonist.gender.pronounPossessive } motivation for exploring further. ${
			location.surroundingBiomes.view
				.map { case (dir, biome) => s"to the $dir from this location is $biome" }
				.mkString(" and ").capitalize }. Be sure to include a description of the available exits, so that the player may access them later.",
				Single(System(setting.systemMessageIncludingProtagonist)))
			.tryFlatMapSuccess(printAndReturnMessage)
	}
	
	private def printAndReturnMessage(reply: ReplyMessage) = {
		reply.printAsReceived()
		reply.future.mapIfSuccess { _.message }
	}
}
