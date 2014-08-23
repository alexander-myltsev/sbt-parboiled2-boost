package sbtparboiled2boost

import scala.util.{Failure, Success}
import org.parboiled2._

object Generator {
  def generateFromParboiledGrammar(template: String, expandTo: Int): String = {
    //generate(TemplateParser.parse(template))(expandTo)
    val parser = new ParboiledParser(template)
    parser.InputLine.run() match {
      case Success(exprAst)       => "Result: " + exprAst
      case Failure(e: ParseError) => "Expression is not valid: " + parser.formatError(e)
      case Failure(e)             => "Unexpected error during parsing run: " + e
    }
  }
}
