package vf.voyage.controller.ollama

import utopia.echo.controller.OllamaClient
import utopia.echo.model.LlmDesignator
import utopia.echo.model.enumeration.ModelParameter
import utopia.echo.model.enumeration.ModelParameter.{MiroStat, MiroStatTau, Temperature, TopK, TopP}
import utopia.echo.model.request.generate.Prompt
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.model.immutable.Value
import utopia.flow.view.template.Extender
import vf.voyage.controller.Common._

/**
 * An interface for making requests to the Ollama API
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
object VoyageOllamaClient extends Extender[OllamaClient]
{
	// ATTRIBUTES   ------------------------
	
	override val wrapped = new OllamaClient()
	
	/**
	 * The default options to use in response-generation
	 */
	val options = Map[ModelParameter, Value](
		Temperature -> 0.9, MiroStat -> 2, MiroStatTau -> 8.0, TopK -> 70, TopP -> 0.95)
	
	
	// OTHER    ----------------------------
	
	/**
	 * Generates a streamed response based on the specified prompt
	 * @param prompt A prompt to apply
	 * @param llm Targeted LLM
	 * @return A future that resolves into a streamed request result
	 */
	def generate(prompt: Prompt)(implicit llm: LlmDesignator) =
		wrapped.push(prompt.toQuery.toRequestParams.withOptions(options).toRequest.streamed).future
}
