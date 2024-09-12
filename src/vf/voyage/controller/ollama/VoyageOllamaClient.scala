package vf.voyage.controller.ollama

import utopia.annex.model.response.{RequestFailure, RequestResult, Response}
import utopia.echo.controller.OllamaClient
import utopia.echo.model.ChatMessage
import utopia.echo.model.enumeration.ChatRole.System
import utopia.echo.model.enumeration.ModelParameter
import utopia.echo.model.enumeration.ModelParameter._
import utopia.echo.model.request.chat.ChatRequest
import utopia.echo.model.request.chat.ChatRequest.ChatRequestFactory
import utopia.echo.model.request.generate.{GenerateParams, Prompt, Query}
import utopia.echo.model.response.OllamaResponse
import utopia.flow.async.AsyncExtensions._
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Empty, Pair}
import utopia.flow.generic.casting.ValueConversions._
import utopia.flow.generic.model.immutable.Value
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.string.Regex
import utopia.flow.time.TimeExtensions._
import utopia.flow.util.logging.FileLogger
import utopia.flow.util.StringExtensions._
import utopia.flow.view.template.Extender
import vf.voyage.controller.Common._
import vf.voyage.model.context.{CharacterDescription, Gf}

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
	
	private val boldRegex = Regex.escape('*').times(2)
	private val fourWhiteSpacesRegex = Regex.whiteSpace.times(4)
	
	override val wrapped = new OllamaClient()
	
	private lazy val debugLogger = new FileLogger("data/log", 1.seconds)
	
	/**
	 * The default options to use in response-generation
	 */
	val options = Map[ModelParameter, Value](
		Temperature -> 1.0, MiroStat -> 1, MiroStatTau -> 8.0, TopK -> 80, TopP -> 0.95)
	
	
	// OTHER    ----------------------------
	
	/**
	 * Generates a streamed response based on the specified prompt
	 * @param prompt A prompt to apply
	 * @param gf The game facilitator
	 * @param protagonist The game's protagonist, if that information is available
	 * @return A future that resolves into a streamed request result
	 */
	def generate(prompt: Prompt, options: Map[ModelParameter, Value] = Map())
	            (implicit gf: Gf, protagonist: CharacterDescription) =
	{
		val params = gfInstructionToPrompt(prompt).toQuery.toRequestParams.withOptions(this.options ++ options)
		val future = wrapped.push(params.toRequest.streamed).future
		
		// Performs some logging
		logQuery(params, future)
		future
	}
	/**
	 * Generates a response without streaming
	 * @param query Query to for the targeted LLM
	 * @param options Additional options for the generation process
	 * @param gf Implicit game facilitator
	 * @param protagonist The game's protagonist, if that information is available
	 * @return Future that resolves into the acquired reply response
	 */
	def generateBuffered(query: Query, options: Map[ModelParameter, Value] = Map())
	                    (implicit gf: Gf, protagonist: CharacterDescription) =
	{
		val params = query.mapPrompt { gfInstructionToPrompt(_) }.toRequestParams.withOptions(this.options ++ options)
		val future = wrapped.push(params.toRequest.buffered).future
		
		logQuery(params, future)
		future
	}
	
	/**
	 * Sends a chat message to the LLM
	 * @param message Message to send
	 * @param messageHistory Message history (default = empty)
	 * @param options Additional options for the LLM (default = empty)
	 * @param gf The game facilitator
	 * @param protagonist The game's protagonist, if that information is available
	 * @return A future that resolves into LLM's reply
	 */
	def chat(message: String, messageHistory: Seq[ChatMessage] = Empty, options: Map[ModelParameter, Value] = Map())
	        (implicit gf: Gf, protagonist: CharacterDescription) =
		_chat(message, messageHistory, options) { _.streamed }
	/**
	 * Sends a chat message to the LLM. Receives the whole response at once.
	 * @param message Message to send
	 * @param messageHistory Message history (default = empty)
	 * @param options Additional options for the LLM (default = empty)
	 * @param gf The game facilitator
	 * @param protagonist The game's protagonist, if that information is available
	 * @return A future that resolves into LLM's reply
	 */
	def chatBuffered(message: String, messageHistory: Seq[ChatMessage] = Empty, options: Map[ModelParameter, Value])
	                (implicit gf: Gf, protagonist: CharacterDescription) =
		_chat(message, messageHistory, options) { _.buffered }
	
	/**
	 * Prints the LLM's reply as it is being received and returns the final text.
	 * @param reply Reply received from Ollama
	 * @param clean Whether bolding should be removed and some empty lines removed from the final text (default = false)
	 * @return Future that resolves into the final text, or a failure
	 */
	def printAndReturnText(reply: OllamaResponse, clean: Boolean = false) = {
		reply.printAsReceived()
		reply.future.mapIfSuccess { r => if (clean) cleanText(r.text) else r.text }
	}
	
	/**
	 * Parses a list of indexed entries. Useful for interpreting queries that request a list of options.
	 * Prints the response as it is being received.
	 * @param reply Reply to interpret
	 * @return Future that resolves into the parsed entries, or a failure.
	 */
	def parseIndexedList(reply: OllamaResponse) = {
		reply.printAsReceived()
		reply.future
			.mapIfSuccess {
				_.text.linesIterator.filter { _.take(10).exists { _.isDigit } }
					.map { line =>
						line.drop(line.indexWhere { _.isDigit }).dropWhile { c => !c.isLetter }
							.replaceEachMatchOf(boldRegex, "").trim
					}
					.toVector
			}
	}
	
	/**
	 * Extracts an integer number from an LLM reply. Expects a very short reply.
	 * @param reply Received reply
	 * @return Future that resolves into an integer, if generation & parsing succeeds
	 */
	def extractInt(reply: OllamaResponse) =
		reply.future.tryMapIfSuccess { reply =>
			reply.text.dropWhile { !_.isDigit }.takeWhile { _.isDigit }.int
				.orElse {
					Vector("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7,
						"eight" -> 8, "nine" -> 9)
						.findMap { case (str, num) => if (reply.text.contains(str)) Some(num) else None }
				}
				.toTry { new IllegalArgumentException(s"Can't extract an integer from \"${ reply.text }\"") }
		}
	
	private def cleanText(text: String) = {
		val lines = text.linesIterator.toVector
		// Removes some of the empty lines
		val removedIndices = lines.indices.toIntSet
			.filter { i =>
				if (lines(i).isEmpty) {
					if (i == 0 || i == lines.size - 1)
						true
					else {
						val surroundingLines = Pair(i - 1, i + 1).map(lines.apply)
						if (surroundingLines.exists { _.isEmpty })
							true
						else if (surroundingLines.first.length < 64 ||
							Pair("*", "-").exists(surroundingLines.second.startsWith))
							true
						else
							false
					}
				}
				else
					false
			}
		lines.view.zipWithIndex.filterNot { case (_, i) => removedIndices.contains(i) }
			// Removes **bold** statements and converts certain whitespace-sequences to tabs instead
			.map { case (line, _) =>
				line.replaceEachMatchOf(boldRegex, "").replaceEachMatchOf(fourWhiteSpacesRegex, "\t").trim
			}
			.mkString("\n")
	}
	
	// Supports both buffered and streamed chat requests
	private def _chat[A <: OllamaResponse](message: String, messageHistory: Seq[ChatMessage] = Empty,
	                                       options: Map[ModelParameter, Value] = Map())
	                                      (createRequest: ChatRequestFactory => ChatRequest[A])
	                                      (implicit gf: Gf, protagonist: CharacterDescription) =
	{
		val params = ChatMessage(message).toRequestParams
			.withConversationHistory(System(gf.systemMessage) +: messageHistory)
			.withOptions(this.options ++ options)
		val future = wrapped.push(createRequest(params.toRequest)).future
		
		// Performs some logging
		debugLogger(s"---------------------\n${ params.messages.mkString("\n") }")
		logResponse(future)
		
		future
	}
	
	private def gfInstructionToPrompt(prompt: Prompt)(implicit gf: Gf, protagonist: CharacterDescription) =
		prompt.mapSystemMessage { msg =>
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
					debugLogger(s"\n${reply.text}")
					reply.statisticsFuture.foreach {
						case Success(statistics) => debugLogger(statistics.toString)
						case Failure(error) => debugLogger(error, "Failed to acquire statistics")
					}
				case Failure(error) => debugLogger(error, "Response-processing failed")
			}
		case f: RequestFailure => debugLogger(f.cause)
	}
}
