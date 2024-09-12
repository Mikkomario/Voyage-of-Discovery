package vf.voyage.model.context

import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.template.ModelConvertible
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.casting.ValueUnwraps._
import utopia.flow.generic.factory.FromModelFactoryWithSchema
import utopia.flow.generic.model.mutable.DataType.StringType
import utopia.flow.util.StringExtensions._

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
	// ATTRIBUTES --------------------------
	
	/**
	 * @return A system message which describes this setting to an LLM
	 */
	lazy val systemMessage = s"Game's genre: ${genre.endingWith(".")}. \nGame's theme: ${
		theme.endingWith(".")} \nGame's general environment: ${worldDescription.endingWith(".")}"
	
	
	// COMPUTED --------------------------
	
	/**
	 * A system message which describes this setting to an LLM.
	 * Includes description of the game's protagonist
	 * @param protagonist Game's protagonist (implicit)
	 * @return Description of the game setting, including protagonist's description
	 */
	def systemMessageIncludingProtagonist(implicit protagonist: CharacterDescription) =
		s"$systemMessage.\nGame's protagonist: ${ protagonist.name }: ${
			protagonist.description.endingWith(".")}"
	
	
	// IMPLEMENTED  ----------------------
	
	override def toModel: Model = Model.from("genre" -> genre, "theme" -> theme, "world" -> worldDescription)
}