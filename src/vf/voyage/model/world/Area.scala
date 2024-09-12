package vf.voyage.model.world

import utopia.echo.model.ChatMessage
import utopia.flow.generic.model.immutable.Model
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.generic.casting.ValueConversions._
import vf.voyage.model.enumeration.CompassDirection

/**
 * Represents a single explorable area within the game. Parts of this area's information are added lazily.
 * @param biome A short description of this area's environmental style
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
class Area(val biome: String, val blockedDirections: Set[CompassDirection]) extends ModelConvertible
{
	// ATTRIBUTES   ---------------------------
	
	/**
	 * Directions that may be traversed towards from this area
	 */
	lazy val accessibleDirections = CompassDirection.valueSet -- blockedDirections
	
	// Will store the story that unfolds within this area
	private var story = Seq[ChatMessage]()
	
	
	// COMPUTED -------------------------------
	
	def explored = story.nonEmpty
	
	
	// IMPLEMENTED  --------------------------
	
	override def toModel: Model = Model.from(
		"biome" -> biome,
		"blockedDirections" -> blockedDirections.view.map { _.toString }.toVector,
		"story" -> story)
}
