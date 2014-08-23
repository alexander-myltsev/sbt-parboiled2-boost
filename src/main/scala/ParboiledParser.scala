package sbtparboiled2boost

import org.parboiled2._

object ParboiledParser {
  sealed trait Expr
  case class Value(value: String) extends Expr
  case class Addition(lhs: Expr, rhs: Expr) extends Expr
  case class Subtraction(lhs: Expr, rhs: Expr) extends Expr
  case class Multiplication(lhs: Expr, rhs: Expr) extends Expr
  case class Division(lhs: Expr, rhs: Expr) extends Expr
}

class ParboiledParser(val input: ParserInput) extends Parser {
  import ParboiledParser._
  
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Expr] = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> Addition
        | '-' ~ Term ~> Subtraction)
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> Multiplication
        | '/' ~ Factor ~> Division)
  }

  def Factor = rule { Number | Parens }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Number = rule { capture(Digits) ~> Value }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}
