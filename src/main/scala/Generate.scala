package sbtparboiled2boost

import scala.util.{Failure, Success}
import org.parboiled2._
import ParboiledParser._
import optimizers.SquashStrings

object Generator {
  def generateFromParboiledGrammar(name: String, template: String): String = {
    val parser = new ParboiledParser(template)

    parser.InputLine.run() match {
      case Success(exprAst) =>
        val treeOptimized = SquashStrings.optimize(exprAst)

        s"""import org.parboiled2._
          |
          |class $name(val input: ParserInput) extends Parser {
          |
          |${treeOptimized.map(ParboiledParser.pretty).mkString("\n\n")}
          |
          |}
          |""".stripMargin
      case Failure(e: ParseError) => throw new Exception("Expression is not valid: " + parser.formatError(e))
      case Failure(e) => throw new Exception("Unexpected error during parsing run: " + e)
    }
  }
}
