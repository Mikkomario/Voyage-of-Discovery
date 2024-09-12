package vf.voyage.model.enumeration

import utopia.flow.util.StringExtensions._
import vf.voyage.model.context.{CharacterDescription, Player}

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
	 * @param character Description of the game's protagonist. Empty if character-creation has not yet been completed.
	 * @return A system message instructing an LLM to function in this role
	 */
	def systemMessage(player: Player)(implicit character: CharacterDescription): String
}

object GfRole
{
	// ATTRIBUTES   -------------------
	
	/**
	 * All available GF roles
	 */
	val values = Vector[GfRole](Facilitator, Designer, Narrator)
	
	
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
		
		override def systemMessage(player: Player)(implicit character: CharacterDescription): String = {
			val charNameStr = character.name.prependIfNotEmpty("as ")
			s"You are a facilitator of a role-playing game. User's name is $player and ${
				player.gender.pronoun}'s playing the game$charNameStr. Your role is to make ${
				player.gender.pronounPossessive } role-playing experience interesting and immersive."
		}
	}
	
	case object Designer extends GfRole
	{
		override def name: String = "designer"
		
		override def systemMessage(player: Player)(implicit character: CharacterDescription): String =
			"You're a creative assistant who helps the user to design an immersive role-playing game."
	}
	
	case object Narrator extends GfRole
	{
		override def name: String = "narrator"
		
		override def systemMessage(player: Player)(implicit character: CharacterDescription): String =
			s"You are a story-teller in a role-playing game. The name of the game's player is ${ player.name } and ${
				player.gender.pronoun }'s playing as ${
				character.name }, the game's protagonist. Your task is to describe how the protagonist experiences the game world and to narrate ${
				character.gender.pronounPossessive } actions and their consequences. If the scene involves other active parties, also describe their responses and actions during and/or immediately after those of the protagonist. Since this game doesn't have any visual elements in it, it is important to provide the narration in enough detail, so that ${
				player.name } can accurately imagine the game's environment, its actors and its obstacles. Describe the environment and the events from ${
				character.name }'s perspective, but speak of ${
				character.gender.pronounObject } in the third person; Focus on the aspects that ${
				character.gender.pronoun } would focus on and leave out things which ${
				character.gender.pronoun } would not perceive."
	}
}
