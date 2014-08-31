package sbtparboiled2boost

import scala.util.{Failure, Success}
import org.parboiled2._

object Generator {
  def generateFromParboiledGrammar(template: String, expandTo: Int): String = {
    val parser = new ParboiledParser(template)
    parser.InputLine.run() match {
      case Success(exprAst) => 
        s"""
          |class ParboiledParser(val input: ParserInput) extends Parser {
          |  ${exprAst.map(ParboiledParser.pretty).mkString("\n\n")}
          |}
        """.stripMargin
      case Failure(e: ParseError) => throw new Exception("Expression is not valid: " + parser.formatError(e))
      case Failure(e)             => throw new Exception("Unexpected error during parsing run: " + e)
    }
  }
}
