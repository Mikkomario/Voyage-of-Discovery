package vf.voyage.model.context

import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.casting.ValueUnwraps._
import utopia.flow.generic.factory.{FromModelFactory, FromModelFactoryWithSchema}
import utopia.flow.generic.model.mutable.DataType.StringType

object CharacterDescription extends FromModelFactoryWithSchema[CharacterDescription]
{
	// ATTRIBUTES   ---------------------
	
	override lazy val schema: ModelDeclaration = ModelDeclaration("name" -> StringType, "description" -> StringType)
	
	
	// IMPLEMENTED  ---------------------
	
	override protected def fromValidatedModel(model: Model): CharacterDescription =
		apply(model("name"), model("description"))
}

/**
 * Describes a game character
 *
 * @param name Name of this character
 * @param description Description of this character
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
case class CharacterDescription(name: String, description: String) extends ModelConvertible
{
	override def toModel: Model = Model.from("name" -> name, "description" -> description)
}
