package vf.voyage.model.enumeration

import utopia.flow.collection.immutable.Pair
import utopia.flow.operator.enumeration.Binary

/**
 * An enumeration for gender
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
sealed trait Gender extends Binary[Gender]
{
	// ABSTRACT -------------------------
	
	/**
	 * @return Name of this gender
	 */
	def name: String
	
	/**
	 * @return The singular pronoun used for a person with this gender (i.e. he or she)
	 */
	def pronoun: String
	/**
	 * @return Possessive form of this gender's pronoun (i.e. his or her).
	 */
	def pronounPossessive: String
	/**
	 * @return A noun (?) form of this gender (i.e. him or her)
	 */
	def pronounObject: String
	
	
	// IMPLEMENTED  ---------------------
	
	override def self = this
	
	override def toString = name
}

object Gender
{
	// ATTRIBUTES   --------------------
	
	/**
	 * All available genders
	 */
	val values = Pair(Male, Female)
	
	
	// VALUES   ------------------------
	
	case object Male extends Gender
	{
		override def name = "male"
		override def pronoun = "he"
		override def pronounPossessive: String = "his"
		override def pronounObject: String = "him"
		
		override def unary_- = Female
		
		override def compareTo(o: Gender) = if (o == Male) 0 else 1
	}
	
	case object Female extends Gender
	{
		override def name = "female"
		override def pronoun = "she"
		override def pronounPossessive: String = "her"
		override def pronounObject: String = "her"
		
		override def unary_- = Male
		
		override def compareTo(o: Gender) = if (o == Female) 0 else -1
	}
}
