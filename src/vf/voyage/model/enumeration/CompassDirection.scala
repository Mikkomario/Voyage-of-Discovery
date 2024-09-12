package vf.voyage.model.enumeration

import utopia.flow.collection.CollectionExtensions._
import utopia.flow.operator.equality.EqualsExtensions._
import utopia.flow.operator.Reversible

import scala.util.Random

/**
 * Enumeration for the four cardinal directions
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
sealed trait CompassDirection extends Reversible[CompassDirection]
{
	// COMPUTED -----------------------
	
	def opposite = -this
	
	
	// IMPLEMENTED  -------------------
	
	override def self: CompassDirection = this
	
	override def toString = super.toString.toLowerCase
}

object CompassDirection
{
	// ATTRIBUTES   -----------------------
	
	val values = Vector[CompassDirection](North, East, South, West)
	val valueSet = values.toSet
	
	
	// COMPUTED ---------------------------
	
	def random = values.random
	
	def randomized = Random.shuffle(values)
	
	
	// OTHER    ---------------------------
	
	def forName(name: String) =
		values.find { _.toString ~== name }
			.orElse {
				if (name.isEmpty)
					None
				else
					name.head.toLower match {
						case 'n' => Some(North)
						case 'e' => Some(East)
						case 's' => Some(South)
						case 'w' => Some(West)
						case _ => None
					}
			}
			.toTry { new NoSuchElementException(s"None of the compass directions matches '$name'") }
	
	def takeRandom(count: Int) = randomized.take(count)
	
	
	// VALUES   ---------------------------
	
	case object North extends CompassDirection
	{
		override def unary_- : CompassDirection = South
	}
	
	case object South extends CompassDirection
	{
		override def unary_- : CompassDirection = North
	}
	
	case object East extends CompassDirection
	{
		override def unary_- : CompassDirection = West
	}
	
	case object West extends CompassDirection
	{
		override def unary_- : CompassDirection = East
	}
}