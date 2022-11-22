package simulator

import scala.collection.mutable.Buffer
import io.circe.generic.auto._, io.circe.parser.decode

object FileReader {

  /** Returns the bodies specified in the JSON file in a buffer */
  def readFile(file: String) = {
    def readText(source: scala.io.Source) = source.getLines().mkString("\n")
    def readJson(text: String) = decode[List[Body]](text).toTry
    val fileSource = util.Try(scala.io.Source.fromFile(file))
    val bodies = Buffer[Body]()
    for {
      source <- fileSource
      text = readText(source)
      list <- readJson(text)
      body <- list
    } bodies += body
    if (bodies.isEmpty) {
      throw new InvalidInputException("Invalid file name or format.")
    }
    bodies
  }

}
