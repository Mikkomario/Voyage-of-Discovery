package vf.voyage.model.world

import utopia.echo.model.ChatMessage
import utopia.flow.view.immutable.caching.Lazy

/**
 * Represents a single explorable area within the game. Parts of this area's information are added lazily.
 * @param biome A short description of this area's environmental style
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
class Area(val biome: String)
{
	// ATTRIBUTES   ---------------------------
	
	// Will store the story that unfolds within this area
	private var story = Seq[ChatMessage]()
	
	
	// COMPUTED -------------------------------
	
	def explored = story.nonEmpty
	
	
	// OTHER    -------------------------------
	
	def enter() = {
		// TODO: Implement
	}
	
	private def enterFirstTime() = {
	
	}
}
