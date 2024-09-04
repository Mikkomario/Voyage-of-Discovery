package vf.voyage.model.context

import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.template.{ModelConvertible, ModelLike, Property}
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.factory.FromModelFactory
import utopia.flow.generic.model.mutable.DataType.StringType
import vf.voyage.model.enumeration.Gender

import scala.util.Try

object Player extends FromModelFactory[Player]
{
	// ATTRIBUTES   --------------------
	
	private lazy val schema = ModelDeclaration("name" -> StringType, "gender" -> StringType)
	
	
	// IMPLEMENTED  --------------------
	
	override def apply(model: ModelLike[Property]): Try[Player] = schema.validate(model).flatMap { model =>
		Gender.forName(model("gender").getString).map { apply(model("name").getString, _) }
	}
}

/**
 * Contains basic information about the player
 * @param name Name of the player
 * @param gender Player's gender
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
case class Player(name: String, gender: Gender) extends ModelConvertible
{
	override def toString = name
	
	override def toModel: Model = Model.from("name" -> name, "gender" -> gender.name)
}