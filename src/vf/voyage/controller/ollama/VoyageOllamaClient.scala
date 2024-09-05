package vf.voyage.controller.ollama

import utopia.annex.model.response.{RequestFailure, RequestResult, Response}
import utopia.echo.controller.OllamaClient
import utopia.echo.model.ChatMessage
import utopia.echo.model.enumeration.ChatRole.System
import utopia.echo.model.enumeration.ModelParameter
import utopia.echo.model.enumeration.ModelParameter._
import utopia.echo.model.request.generate.{GenerateParams, Prompt, Query}
import utopia.echo.model.response.OllamaResponse
import utopia.flow.collection.immutable.Empty
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.model.immutable.Value
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.time.TimeExtensions._
import utopia.flow.util.logging.FileLogger
import utopia.flow.view.template.Extender
import vf.voyage.controller.Common._
import vf.voyage.model.context.Gf

import scala.concurrent.Future
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
	
	/**
	 * Generates a streamed response based on the specified prompt
	 * @param prompt A prompt to apply
	 * @param gf The game facilitator
	 * @return A future that resolves into a streamed request result
	 */
	def generate(prompt: Prompt)(implicit gf: Gf) = {
		val params = gfInstructionToPrompt(prompt).toQuery.toRequestParams.withOptions(options)
		val future = wrapped.push(params.toRequest.streamed).future
		
		// Performs some logging
		logQuery(params, future)
		future
	}
	/**
	 * Generates a response without streaming
	 * @param query Query to for the targeted LLM
	 * @param gf Implicit game facilitator
	 * @return Future that resolves into the acquired reply response
	 */
	def generateBuffered(query: Query)(implicit gf: Gf) = {
		val params = query.mapPrompt { gfInstructionToPrompt(_) }.toRequestParams.withOptions(options)
		val future = wrapped.push(params.toRequest.buffered).future
		
		logQuery(params, future)
		future
	}
	
	/**
	 * Sends a chat message to the LLM
	 * @param message Message to send
	 * @param messageHistory Message history (default = empty)
	 * @param gf The game facilitator
	 * @return A future that resolves into LLM's reply
	 */
	def chat(message: String, messageHistory: Seq[ChatMessage] = Empty)(implicit gf: Gf) = {
		val request = ChatMessage(message).toRequestParams
			.withConversationHistory(System(gf.systemMessage) +: messageHistory)
			.withOptions(options)
			.toRequest.streamed
		val future = wrapped.push(request).future
		
		// Performs some logging
		debugLogger(s"---------------------\n${ request.params.messages.mkString("\n") }")
		logResponse(future)
		
		future
	}
	
	private def gfInstructionToPrompt(prompt: Prompt)(implicit gf: Gf) = prompt.mapSystemMessage { msg =>
		if (msg.isEmpty)
			gf.systemMessage
		else
			s"${ gf.systemMessage }\n\n$msg"
	}
	
	private def logQuery(params: GenerateParams, responseFuture: Future[RequestResult[OllamaResponse]]) = {
		val query = params.query
		debugLogger(s"---------------------\nSystem: ${ query.toSystem }\nPrompt: ${ query.toPrompt }\nContextual: ${
			params.conversationContext.nonEmpty }")
		logResponse(responseFuture)
	}
	
	private def logResponse(responseFuture: Future[RequestResult[OllamaResponse]]) = responseFuture.foreach {
		case Response.Success(reply, _, _) =>
			reply.future.foreach {
				case Success(reply) =>
					debugLogger(reply.text)
					reply.statisticsFuture.foreach {
						case Success(statistics) => debugLogger(statistics.toString)
						case Failure(error) => debugLogger(error, "Failed to acquire statistics")
					}
				case Failure(error) => debugLogger(error, "Response-processing failed")
			}
		case f: RequestFailure => debugLogger(f.cause)
	}
}
