package sbtparboiled2boost

import org.parboiled2._

object ParboiledParser {
  sealed trait Expr
  case class Sequence(lhs: Expr, rhs: Expr) extends Expr
  case class FirstOf(lhs: Expr, rhs: Expr) extends Expr
  case class RuleExpr(name: Identifier, args: Option[Seq[Identifier]], typ: Option[Identifier], body: Expr) extends Expr
  case class Identifier(name: String) extends Expr
  case class StringLiteral(v: String) extends Expr
  case class Capture(e: Expr) extends Expr
  case class Optional(e: Expr) extends Expr
  case class ZeroOrMore(e: Expr) extends Expr
  case class OneOrMore(e: Expr) extends Expr
  case class Test(e: Expr) extends Expr
  case class Negate(e: Expr) extends Expr
  case class RuleCallExpr(name: Identifier, args: Seq[Identifier]) extends Expr

  def pretty(expr: Expr): String = expr match {
    case RuleExpr(name, args, typ, body) =>
      "def " + pretty(name) +
        args.map{_.map(pretty).mkString("(", ", ", ")")}.getOrElse("") +
        typ.map{x => s": ${pretty(x)}"}.getOrElse("") +
        " = rule { " + pretty(body) + " }"

    case Identifier(name: String) => name
    case FirstOf(lhs, rhs)        => pretty(lhs) + " | " + pretty(rhs)
    case Sequence(lhs, rhs)       => pretty(lhs) + " ~ " + pretty(rhs)
    case Capture(e)               => "capture(" + pretty(e) + ")"
    case Optional(e)              => "optional(" + pretty(e) + ")"
    case ZeroOrMore(e)            => "zeroOrMore(" + pretty(e) + ")"
    case OneOrMore(e)             => "oneOrMore(" + pretty(e) + ")"
    case StringLiteral(v)         => '"' + v + '"'
    case Test(e)                  => "&(" + pretty(e) + ")"
    case Negate(e)                => "!(" + pretty(e) + ")"
    case RuleCallExpr(name, ids)  => pretty(name) + "(" + ids.map(pretty).mkString(", ") + ")"
  }
}

class ParboiledParser(val input: ParserInput) extends Parser {
  import ParboiledParser._

  import CharPredicate.{Alpha, AlphaNum, Visible}
  
  def InputLine = rule { Expression ~ EOI }

  def Expression = rule { zeroOrMore(RuleDef ~ WhiteSpace) }
  
  def RuleDef = rule { Ident ~ optional("(" ~ oneOrMore(Ident) ~ ")") ~ WhiteSpace ~ optional(":" ~ Type) ~ "::=" ~ Body ~> RuleExpr }
  
  def Type = rule { WhiteSpace ~ capture(Alpha ~ zeroOrMore(AlphaNum | "[" | "]")) ~ WhiteSpace ~> Identifier }
  
  def Body: Rule1[Expr] = rule { SeqRule ~ zeroOrMore("|" ~ SeqRule ~> FirstOf) }
  
  def SeqRule: Rule1[Expr] = rule { SimpleExpr ~ zeroOrMore("~" ~ SimpleExpr ~> Sequence) }

  def SimpleExpr: Rule1[Expr] = rule { RuleCall | TestRule | NegateRule | OneOrMoreRule | ZeroOrMoreRule |
                                       OptionalRule | CaptureRule | StringLiteralRule | Ident | ('(' ~ Body ~ ')') }

  def RuleCall: Rule1[RuleCallExpr] = rule { Ident ~ "(" ~ optional(Ident) ~ zeroOrMore("," ~ Ident) ~ ")" ~ WhiteSpace ~> (
    (name: Identifier, argOpt: Option[Identifier], args: Seq[Identifier]) =>
      RuleCallExpr(name, argOpt.map{x => x +: args}.getOrElse(args))) }

  def CaptureRule = rule { WhiteSpace ~ "capture(" ~ Body ~ ")" ~ WhiteSpace ~> Capture }
  
  def OptionalRule = rule { WhiteSpace ~ "optional(" ~ Body ~ ")" ~ WhiteSpace ~> Optional }
  
  def ZeroOrMoreRule = rule { WhiteSpace ~ "zeroOrMore(" ~ Body ~ ")" ~ WhiteSpace ~> ZeroOrMore }
  
  def OneOrMoreRule = rule { WhiteSpace ~ "oneOrMore(" ~ Body ~ ")" ~ WhiteSpace ~> OneOrMore }
  
  def StringLiteralRule = rule { WhiteSpace ~ '"' ~ capture(zeroOrMore(Visible -- "\"")) ~ '"' ~ WhiteSpace ~> StringLiteral }
  
  def TestRule = rule { WhiteSpace ~ "&(" ~ Body ~ ")" ~ WhiteSpace ~> Test }
  
  def NegateRule = rule { WhiteSpace ~ "!(" ~ Body ~ ")" ~ WhiteSpace ~> Negate }
  
  def Ident = rule { WhiteSpace ~ capture(Alpha ~ zeroOrMore(AlphaNum)) ~ WhiteSpace ~> Identifier }

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }
  
  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
}

object ParboiledParserTest {  
  import scala.util.{Failure, Success}
  import org.parboiled2._

  def parse(input: String) = {
    val parser = new ParboiledParser(input)
    println("-----")
    parser.InputLine.run() match {
      case Success(exprAst)       => println("Result:\n" + exprAst.map(ParboiledParser.pretty).mkString("\n"))
      case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
      case Failure(e)             => println("Unexpected error during parsing run: " + e)
    }
  }
  
  def main(args: Array[String]): Unit = {
    parse(
      """def a = !(oneOrMore(abc) ~ edf | k) ~ optional(edf)
        |""".stripMargin)
  }
}
