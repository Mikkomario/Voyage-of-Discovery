package vf.voyage.model.enumeration

import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.operator.enumeration.Binary

/**
 * An enumeration for gender
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
sealed trait Gender
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
	
	/**
	 * @return This gender's masculinity, i.e. "masculine" or "feminine".
	 */
	def masculinity: String
	
	/**
	 * @return Either male or female. None if undefined.
	 */
	def binary: Option[BinaryGender]
	
	
	// IMPLEMENTED  ---------------------
	
	override def toString = name
}

sealed trait BinaryGender extends Gender with Binary[BinaryGender]
{
	override def self: BinaryGender = this
	override def binary = Some(this)
}

object Gender
{
	// ATTRIBUTES   --------------------
	
	/**
	 * Male and female
	 */
	val binaryValues = Pair[Gender](Male, Female)
	/**
	 * Male, female and undefined
	 */
	val values = binaryValues :+ Undefined
	
	
	// OTHER    ------------------------
	
	def findForName(genderName: String) = binaryValues.find { _.name == genderName }
	def forName(genderName: String) =
		findForName(genderName)
			.toTry { new NoSuchElementException(s"None of the available genders matches '$genderName'") }
	
	
	// VALUES   ------------------------
	
	case object Male extends BinaryGender
	{
		override def name = "male"
		override def pronoun = "he"
		override def pronounPossessive: String = "his"
		override def pronounObject: String = "him"
		override def masculinity: String = "masculine"
		
		override def unary_- = Female
		
		override def compareTo(o: BinaryGender) = if (o == Male) 0 else 1
	}
	
	case object Female extends BinaryGender
	{
		override def name = "female"
		override def pronoun = "she"
		override def pronounPossessive: String = "her"
		override def pronounObject: String = "her"
		override def masculinity: String = "feminine"
		
		override def unary_- = Male
		
		override def compareTo(o: BinaryGender) = if (o == Female) 0 else -1
	}
	
	case object Undefined extends Gender
	{
		override def name = "undefined"
		override def pronoun = "they"
		override def pronounPossessive = "their"
		override def pronounObject = "them"
		override def masculinity = "undefined"
		
		override def binary: Option[BinaryGender] = None
	}
}
