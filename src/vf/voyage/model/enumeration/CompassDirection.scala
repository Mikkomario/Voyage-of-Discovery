package vf.voyage.model.enumeration

import utopia.flow.operator.Reversible

/**
 * Enumeration for the four cardinal directions
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
sealed trait CompassDirection extends Reversible[CompassDirection]
{
	override def self: CompassDirection = this
}

object CompassDirection
{
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