package vf.voyage.model.context

import utopia.echo.model.LlmDesignator

/**
 * Contains information about the game's facilitator (GF), played by an LLM
 * @param llm LLM that plays this role
 * @param name GF's name
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
case class Gf(llm: LlmDesignator, name: String) extends LlmDesignator
{
	// IMPLEMENTED  ----------------------
	
	override def llmName: String = llm.llmName
	
	override def toString = name
}