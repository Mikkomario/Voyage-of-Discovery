package vf.voyage.model.context

import vf.voyage.model.enumeration.Gender

/**
 * Contains basic information about the player
 * @param name Name of the player
 * @param gender Player's gender
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
case class Player(name: String, gender: Gender)
{
	override def toString = name
}