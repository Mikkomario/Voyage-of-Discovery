package vf.voyage.model.world

import utopia.paradigm.shape.template.{Dimensional, Dimensions, DimensionsWrapperFactory, HasDimensions}
import vf.voyage.model.enumeration.CompassDirection
import vf.voyage.model.enumeration.CompassDirection.{East, North, South, West}

object Coordinates extends DimensionsWrapperFactory[Int, Coordinates]
{
	// ATTRIBUTES   -------------------
	
	/**
	 * (0,0) coordinate
	 */
	val origin = twice(0)
	
	
	// IMPLEMENTED  -------------------
	
	override def zeroDimension: Int = 0
	
	override def from(other: HasDimensions[Int]): Coordinates = other match {
		case c: Coordinates => c
		case o => apply(o.dimensions)
	}
}

/**
 * Represents a map "tile" X-Y-coordinate
 *
 * @author Mikko Hilpinen
 * @since 11.09.2024, v0.1
 */
case class Coordinates(dimensions: Dimensions[Int]) extends Dimensional[Int, Coordinates]
{
	// COMPUTED --------------------------
	
	/**
	 * @return This coordinate's neighboring coordinates
	 */
	def neighbors = CompassDirection.values.map { this + _ }
	/**
	 * @return This coordinate's neighboring coordinates
	 */
	def neighborsIterator = CompassDirection.values.iterator.map { this + _ }
	
	
	// IMPLEMENTED  ----------------------
	
	override def self: Coordinates = this
	
	override def withDimensions(newDimensions: Dimensions[Int]): Coordinates = Coordinates(newDimensions)
	
	
	// OTHER    --------------------------
	
	def +(direction: CompassDirection): Coordinates = this + (direction -> 1)
	def +(d: (CompassDirection, Int)) = d._1 match {
		case North => mapY { _ - d._2 }
		case South => mapY { _ + d._2 }
		case East => mapX { _ + d._2 }
		case West => mapX { _ - d._2 }
	}
}