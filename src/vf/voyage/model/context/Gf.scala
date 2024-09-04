package vf.voyage.model.context

import utopia.echo.model.LlmDesignator
import utopia.flow.generic.model.immutable.{Model, ModelDeclaration}
import utopia.flow.generic.model.template.{ModelConvertible, ModelLike, Property}
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.casting.ValueUnwraps._
import utopia.flow.generic.factory.FromModelFactory
import utopia.flow.generic.model.mutable.DataType.{ModelType, StringType}
import vf.voyage.model.enumeration.GfRole

import scala.util.Try

object Gf extends FromModelFactory[Gf]
{
	// ATTRIBUTES   --------------------
	
	private lazy val schema = ModelDeclaration("llm" -> StringType, "name" -> StringType, "player" -> ModelType)
	
	
	// IMPLEMENTED  --------------------
	
	override def apply(model: ModelLike[Property]): Try[Gf] = schema.validate(model).flatMap { model =>
		Player(model("player").getModel).map { player =>
			apply(LlmDesignator(model("llm")), model("name"), player, GfRole.forName(model("role").getString))
		}
	}
}

/**
 * Contains information about the game's facilitator (GF), played by an LLM
 * @param llm LLM that plays this role
 * @param name GF's name
 * @param player Information about the game's player
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
case class Gf(llm: LlmDesignator, name: String, player: Player, role: GfRole)
	extends LlmDesignator with ModelConvertible
{
	// ATTRIBUTES -------------------------
	
	/**
	 * @return An instruction to give to the LLM on how to play their role as the GF.
	 */
	lazy val systemMessage = s"Your name is $name. ${ role.systemMessage(player) }"
	
	
	// IMPLEMENTED  ----------------------
	
	override def llmName: String = llm.llmName
	
	override def toString = name
	override def toModel: Model = Model.from("llm" -> llmName, "name" -> name, "player" -> player, "role" -> role.name)
	
	
	// OTHER    ---------------------------
	
	/**
	 * @param role New role to assign to this GF
	 * @return Copy of this GF with the specified role
	 */
	def withRole(role: GfRole) = copy(role = role)
}