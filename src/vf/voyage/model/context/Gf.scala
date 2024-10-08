package vf.voyage.model.context

import utopia.echo.model.LlmDesignator
import utopia.flow.collection.immutable.caching.cache.CacheLatest
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
	
	private val systemMessageCache =
		CacheLatest { implicit c: CharacterDescription => role.systemMessage(name, player) }
	
	
	// COMPUTED --------------------------
	
	/**
	 * @return The approximate number of tokens int his GF's system message
	 */
	def approxMessageTokens = role.approxMessageTokens + 5
	
	/**
	 * @param character The game's protagonist. Empty if not yet available.
	 * @return A system message instructing the LLM to act in this game facilitator's role
	 */
	def systemMessage(implicit character: CharacterDescription) = systemMessageCache(character)
	
	
	// IMPLEMENTED  ----------------------
	
	override def llmName: String = llm.llmName
	
	override def toString = name
	override def toModel: Model = Model.from("llm" -> llmName, "name" -> name, "player" -> player, "role" -> role.name)
	
	
	// OTHER    ---------------------------
	
	/**
	 * @param role New role to assign to this GF
	 * @return Copy of this GF with the specified role
	 */
	def withRole(role: GfRole) = if (this.role == role) this else copy(role = role)
}