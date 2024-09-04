package vf.voyage.controller

import utopia.access.http.Status
import utopia.bunnymunch.jawn.JsonBunny
import utopia.echo.controller.OllamaClient
import utopia.flow.async.context.ThreadPool
import utopia.flow.parse.json.JsonParser
import utopia.flow.time.TimeExtensions._
import utopia.flow.util.logging.{Logger, SysErrLogger}
import vf.voyage.controller.ollama.VoyageOllamaClient

import scala.concurrent.ExecutionContext
import scala.io.Codec

/**
 * Provides access to commonly used values & objects
 *
 * @author Mikko Hilpinen
 * @since 04.09.2024, v0.1
 */
object Common
{
	// INITIAL CODE --------------------------
	
	Status.setup()
	
	
	// ATTRIBUTES   --------------------------
	
	implicit val codec: Codec = Codec.UTF8
	
	implicit val log: Logger = SysErrLogger
	implicit val jsonParser: JsonParser = JsonBunny
	
	implicit val exc: ExecutionContext = new ThreadPool("Voyage", 3, 200, 30.seconds)
	
	
	// COMPUTED ------------------------------
	
	/**
	 * An interface for sending out Ollama requests
	 */
	def ollama = VoyageOllamaClient
}
