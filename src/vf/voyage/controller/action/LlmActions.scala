package vf.voyage.controller.action

import utopia.annex.model.response.{RequestFailure, Response}
import utopia.annex.util.RequestResultExtensions._
import utopia.echo.model.LlmDesignator
import utopia.echo.model.response.llm.GeneralOllamaModelInfo
import utopia.flow.util.console.ConsoleExtensions._
import vf.voyage.controller.Common._

import scala.io.StdIn

/**
 * Provides interactive actions related to LLMs
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
object LlmActions
{
	/**
	 * Prompts the user to select one local Ollama model
	 * @return Designator of the selected model. None if selection was canceled or if data-reading failed.
	 */
	def selectModel() = {
		ollama.localModels.future.waitForResult() match {
			case Response.Success(models: Seq[GeneralOllamaModelInfo], _, _) =>
				println("Please select the model to use.")
				println("Note: It is recommended to select a model that supports tools")
				StdIn.selectFrom(models.map { m => LlmDesignator(m.name) -> modelLine(m) }, "models", maxListCount = 30)
				
			case f: RequestFailure =>
				log(f.cause, "Failed to retrieve local model data from Ollama")
				None
		}
	}
	
	private def modelLine(model: GeneralOllamaModelInfo) = {
		val sizeStr = model.parameterSize match {
			case Some(size) => s" (${ size }B)"
			case None => ""
		}
		s"${ model.name }$sizeStr"
	}
}
