package vf.voyage.model.context

import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.casting.ValueUnwraps._
import utopia.flow.generic.factory.FromModelFactory
import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.mutable.DataType.StringType
import utopia.flow.generic.model.template.{ModelConvertible, ModelLike, Property}
import utopia.flow.operator.MaybeEmpty
import vf.voyage.model.enumeration.Gender
import vf.voyage.model.enumeration.Gender.Undefined

import scala.util.Try

object CharacterDescription extends FromModelFactory[CharacterDescription]
{
	// ATTRIBUTES   ---------------------
	
	/**
	 * An empty placeholder character description
	 */
	lazy val empty = apply("", "", Undefined)
	
	private lazy val schema: ModelDeclaration =
		ModelDeclaration("name" -> StringType, "description" -> StringType, "gender" -> StringType)
	
	
	// IMPLEMENTED  ---------------------
	
	override def apply(model: ModelLike[Property]): Try[CharacterDescription] = schema.validate(model).flatMap { model =>
		Gender.forName(model("gender")).map { gender =>
			apply(model("name"), model("description"), gender)
		}
	}
}

/**
 * Describes a game character
 *
 * @param name Name of this character
 * @param description Description of this character
 * @param gender This character's gender
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
case class CharacterDescription(name: String, description: String, gender: Gender)
	extends ModelConvertible with MaybeEmpty[CharacterDescription]
{
	// IMPLEMENTED  -------------------------
	
	override def self: CharacterDescription = this
	
	// Empty character descriptions may be used as placeholders before character-creation completes
	override def isEmpty: Boolean = name.isEmpty || description.isEmpty
	
	override def toModel: Model = Model.from("name" -> name, "description" -> description, "gender" -> gender.name)
}
