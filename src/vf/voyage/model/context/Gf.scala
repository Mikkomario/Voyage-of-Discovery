package vf.voyage.model.context

import utopia.echo.model.LlmDesignator

/**
 * Contains information about the game's facilitator (GF), played by an LLM
 * @param llm LLM that plays this role
 * @param name GF's name
 * @param player Information about the game's player
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
case class Gf(llm: LlmDesignator, name: String, player: Player) extends LlmDesignator
{
	// ATTRIBUTES -------------------------
	
	/**
	 * @return An instruction to give to the LLM on how to play their role as the GF.
	 */
	lazy val systemMessage = s"Your name is $name. You are a facilitator of a role-playing game. User's name is $player and ${
		player.gender.pronoun}'s playing the game. Your role is to make ${
		player.gender.pronounPossessive } role-playing experience interesting and immersive."
	
	
	// IMPLEMENTED  ----------------------
	
	override def llmName: String = llm.llmName
	
	override def toString = name
}