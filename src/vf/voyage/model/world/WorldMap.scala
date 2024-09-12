package vf.voyage.model.world

import utopia.annex.util.RequestResultExtensions._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.model.immutable.Model
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.view.mutable.eventful.EventfulPointer
import utopia.flow.view.template.Extender
import vf.voyage.controller.Common._
import vf.voyage.controller.action.WorldBuilder
import vf.voyage.model.context.{CharacterDescription, GameSetting, Gf}
import vf.voyage.model.enumeration.CompassDirection

import scala.util.Random

/**
 * A mutable data structure used for storing information about each world area
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
class WorldMap(startingBiome: String,
               areasSurroundingStart: Iterable[(CompassDirection, String, Set[CompassDirection])])
              (implicit gf: Gf, setting: GameSetting, protagonist: CharacterDescription)
	extends ModelConvertible
{
	// ATTRIBUTES   -----------------------
	
	// Keys are X-Y-pairs. Values are initialized map nodes.
	// Initially this pointer contains the starting area (at (0,0)) and the surrounding accessible areas
	private val gridPointer = EventfulPointer(Map[Coordinates, Area](
		Coordinates.origin ->
			new Area(startingBiome, CompassDirection.valueSet -- areasSurroundingStart.view.map { _._1 }.toSet)) ++
		areasSurroundingStart.map { case (d, biome, exits) =>
			val coordinates = Coordinates.origin + d
			val area = new Area(biome, CompassDirection.valueSet -- exits)
			coordinates -> area
		})
	private val unexploredCoordinatesPointer = gridPointer.map { grid =>
		grid.keysIterator.flatMap { center => center.neighborsIterator.filterNot(grid.contains) }.toSet
	}
	
	private val currentCoordinatesPointer = EventfulPointer(Coordinates.origin)
	private val currentLocationPointer = currentCoordinatesPointer.strongMap { new Location(_) }
	
	
	// COMPUTED ---------------------------
	
	private def grid = gridPointer.value
	private def unexploredCoordinates = unexploredCoordinatesPointer.value
	
	/**
	 * @return The location currently occupied by the protagonist
	 */
	def currentLocation = currentLocationPointer.value
	
	
	// IMPLEMENTED  -----------------------
	
	override def toModel: Model = Model.from(
		"grid" -> grid.view
			.map { case (coordinates, area) => Model.from("location" -> coordinates.xyPair, "area" -> area) }.toVector,
		"current" -> currentLocation.coordinates.xyPair)
	
	
	// OTHER    ---------------------------
	
	private def apply(coordinates: Coordinates) = new Location(coordinates)
	
	// NB: Returned directions are relative to the targeted coordinates
	private def currentEntrancesTo(coordinates: Coordinates) = {
		val grid = this.grid
		// Checks whether that location has already been initialized
		grid.get(coordinates) match {
			// Case: Already initialized => Collects entrances to neighbouring initialized locations
			case Some(area) =>
				area.accessibleDirections.view.map { d => d -> (coordinates + d) }
					.flatMap { case (d, c) => grid.get(c).map { d -> _ } }
					.toMap
			
			// Case: Not yet initialized
			//       => Looks for neighbouring initialized locations that allow access to this location
			case None =>
				CompassDirection.values.iterator
					.flatMap { d =>
						grid.get(coordinates + d).filter { _.accessibleDirections.contains(d.opposite) }.map { d -> _ }
					}
					.toMap
		}
	}
	
	
	// NESTED   ---------------------------
	
	class Location(val coordinates: Coordinates) extends Extender[Area]
	{
		// ATTRIBUTES   -------------------
		
		override lazy val wrapped: Area = gridPointer.mutate { grid =>
			// Checks which existing locations lead to this one already (these must match exits from this location)
			val predefinedEntrances = currentEntrancesTo(coordinates)
			// Possibly generates additional exits, also
			val additionalEntrances = {
				val directionsIter = CompassDirection.randomized.iterator.filterNot(predefinedEntrances.contains)
				// Case: This is the last unexplored location => Forces at least 2 new exits, if at all possible
				if (unexploredCoordinates.only.contains(coordinates)) {
					// NB: This logic may still lead to dead-ends, although it should be quite rare
					val possibleDirections = directionsIter
						.filterNot { d => grid.contains(coordinates + d) }.toOptimizedSeq
					val additionalCount = {
						if (possibleDirections.hasSize > 2)
							2 + Random.nextInt(possibleDirections.size - 1)
						else
							2
					}
					possibleDirections.take(additionalCount).toSet
				}
				// Case: Normal generation => Generates 0-4 new exits randomly
				else
					directionsIter
						.take(Random.nextInt(5 - predefinedEntrances.size))
						.filterNot { d => grid.contains(coordinates + d) }.toSet
			}
			val blockedDirections = CompassDirection.valueSet -- additionalEntrances -- predefinedEntrances.keySet
			
			// Generates this location's biome
			val surroundingBiomes = CompassDirection.values.view
				.flatMap { d =>
					val neighborCoordinates = coordinates + d
					grid.get(neighborCoordinates).map { neighbor =>
						val distantNeighborBiome = grid.get(neighborCoordinates + d) match {
							case Some(a) => a.biome
							case None => ""
						}
						d -> Pair(neighbor.biome, distantNeighborBiome)
					}
				}
				.toMap
			
			println("Just a moment. I need to do some additional world-building...")
			val biome = WorldBuilder.generateBiome(gf, surroundingBiomes, blockedDirections).waitForTry()
				.getOrElseLog { surroundingBiomes.valuesIterator.toOptimizedSeq.random.first }
			
			// Stores the new area in the grid
			val area = new Area(biome, blockedDirections)
			area -> (grid + (coordinates -> area))
		}
		
		/**
		 * A map that lists the biomes that are accessible from this location.
		 * Keys are directions and values are biomes.
		 */
		lazy val surroundingBiomes = wrapped.accessibleDirections.view
			.map { d => d -> WorldMap.this(coordinates + d).biome }.toMap
	}
}
