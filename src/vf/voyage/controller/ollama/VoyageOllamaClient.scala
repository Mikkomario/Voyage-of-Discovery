package vf.voyage.controller.ollama

import utopia.annex.model.response.{RequestFailure, Response}
import utopia.echo.controller.OllamaClient
import utopia.echo.model.ChatMessage
import utopia.echo.model.enumeration.ChatRole.System
import utopia.echo.model.enumeration.ModelParameter
import utopia.echo.model.enumeration.ModelParameter._
import utopia.echo.model.request.generate.Prompt
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.model.immutable.Value
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.time.TimeExtensions._
import utopia.flow.util.logging.FileLogger
import utopia.flow.view.template.Extender
import vf.voyage.controller.Common._
import vf.voyage.model.context.Gf

import scala.util.{Failure, Success}

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
	
	private lazy val debugLogger = new FileLogger("data/log", 1.seconds)
	
	/**
	 * The default options to use in response-generation
	 */
	val options = Map[ModelParameter, Value](
		Temperature -> 1.0, MiroStat -> 2, MiroStatTau -> 10.0, TopK -> 100, TopP -> 0.95)
	
	
	// OTHER    ----------------------------
	// TODO: Add test logging in order to see token usage
	
	/**
	 * Generates a streamed response based on the specified prompt
	 * @param prompt A prompt to apply
	 * @param gf The game facilitator
	 * @return A future that resolves into a streamed request result
	 */
	def generate(prompt: Prompt)(implicit gf: Gf) = {
		val modifiedPrompt = prompt.mapSystemMessage { msg =>
			if (msg.isEmpty)
				gf.systemMessage
			else
				s"${ gf.systemMessage }\n\n$msg"
		}
		val query = modifiedPrompt.toQuery
		val future = wrapped.push(query.toRequestParams.withOptions(options).toRequest.streamed).future
		
		// Performs some logging
		debugLogger(s"---------------------\nSystem: ${ query.toSystem }\nPrompt: ${ query.toPrompt }\nContextual: ${
			query.conversationContext.nonEmpty }")
		future.foreach {
			case Response.Success(reply, _, _) =>
				reply.future.foreach {
					case Success(reply) =>
						debugLogger(reply.text)
						debugLogger(reply.statistics.toString)
					
					case Failure(error) => debugLogger(error, "Response-processing failed")
				}
			case f: RequestFailure => debugLogger(f.cause)
		}
		
		future
	}
	
	/**
	 * Sends a chat message to the LLM
	 * @param message Message to send
	 * @param messageHistory Message history (default = empty)
	 * @param gf The game facilitator
	 * @return A future that resolves into LLM's reply
	 */
	def chat(message: String, messageHistory: Seq[ChatMessage])(implicit gf: Gf) = {
		val request = ChatMessage(message).toRequestParams
			.withConversationHistory(System(gf.systemMessage) +: messageHistory)
			.withOptions(options)
			.toRequest.streamed
		val future = wrapped.push(request).future
		
		// Performs some logging
		debugLogger(s"---------------------\n${ request.params.messages.mkString("\n") }")
		future.foreach {
			case Response.Success(reply, _, _) =>
				reply.future.foreach {
					case Success(reply) =>
						debugLogger(reply.text)
						debugLogger(reply.statistics.toString)
					
					case Failure(error) => debugLogger(error, "Response-processing failed")
				}
			case f: RequestFailure => debugLogger(f.cause)
		}
		
		future
	}
}
