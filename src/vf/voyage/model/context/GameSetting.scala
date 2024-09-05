package vf.voyage.model.context

import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.casting.ValueUnwraps._
import utopia.flow.generic.factory.FromModelFactoryWithSchema
import utopia.flow.generic.model.mutable.DataType.StringType

object GameSetting extends FromModelFactoryWithSchema[GameSetting]
{
	// IMPLEMENTED  ----------------------
	
	override def schema: ModelDeclaration =
		ModelDeclaration("genre" -> StringType, "theme" -> StringType, "world" -> StringType)
	
	override protected def fromValidatedModel(model: Model): GameSetting =
		apply(model("genre"), model("theme"), model("world"))
}

/**
 * Describes the overall setting of the game
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
case class GameSetting(genre: String, theme: String, worldDescription: String) extends ModelConvertible
{
	// COMPUTED --------------------------
	
	/**
	 * @return A system message which describes this setting to an LLM
	 */
	def systemMessage = s"The game's genre is $genre. \nThe theme of the game is $theme. \nHere's a description of the game's environment: $worldDescription"
	
	
	// IMPLEMENTED  ----------------------
	
	override def toModel: Model = Model.from("genre" -> genre, "theme" -> theme, "world" -> worldDescription)
}