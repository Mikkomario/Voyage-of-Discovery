package vf.voyage.model.world

import utopia.flow.view.immutable.caching.Lazy

/**
 * Represents a single explorable area within the game. Parts of this area's information are added lazily.
 * @param biome A short description of this area's environmental style
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
class Area(val biome: String)(generateDescription: => String)
{
	/**
	 * Lazily initialized description of this area
	 */
	val lazyDescription = Lazy { generateDescription }
}
