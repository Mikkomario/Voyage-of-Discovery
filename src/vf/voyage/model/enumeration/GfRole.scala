package vf.voyage.model.enumeration

import utopia.flow.collection.immutable.Pair
import vf.voyage.model.context.Player

/**
 * Various roles an LLM can play within this game and its development
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
sealed trait GfRole
{
	/**
	 * @return Name of this role
	 */
	def name: String
	
	/**
	 * @param player Information about the player of the game
	 * @return A system message instructing an LLM to function in this role
	 */
	def systemMessage(player: Player): String
}

object GfRole
{
	// ATTRIBUTES   -------------------
	
	/**
	 * All available GF roles
	 */
	val values = Pair[GfRole](Facilitator, Designer)
	
	
	// OTHER    -----------------------
	
	/**
	 * @param roleName Name of the targeted role
	 * @return Role with that name. Facilitator if the name didn't match any role.
	 */
	def forName(roleName: String) = values.find { _.name == roleName }.getOrElse(Facilitator)
	
	
	// VALUES   -----------------------
	
	case object Facilitator extends GfRole
	{
		override def name: String = "facilitator"
		
		override def systemMessage(player: Player): String =
			s"You are a facilitator of a role-playing game. User's name is $player and ${
				player.gender.pronoun}'s playing the game. Your role is to make ${
				player.gender.pronounPossessive } role-playing experience interesting and immersive."
	}
	
	case object Designer extends GfRole
	{
		override def name: String = "designed"
		
		override def systemMessage(player: Player): String =
			"You're a creative assistant who helps the user in the design of an immersive role-playing game."
	}
}
