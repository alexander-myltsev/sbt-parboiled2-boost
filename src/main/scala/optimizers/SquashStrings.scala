package optimizers

import sbtparboiled2boost.ParboiledParser._

object SquashStrings {
  def optimize(tree: Seq[Expr]): Seq[Expr] = tree.map {
    case RuleExpr(n, args, t, body) => RuleExpr(n, args, t, squashStrings(body))
    case t => ???
  }

  private def squashStrings(body: Expr): Expr = body match {
    case s @ Sequence(lhs, rhs) =>
      val lhsN = squashStrings(lhs)
      val rhsN = squashStrings(rhs)
      (lhsN, rhsN) match {
        case (StringLiteral(sr), StringLiteral(sl)) => StringLiteral(sr + sl)
        case _ => s
      }

    case FirstOf(lhs, rhs)        => FirstOf(squashStrings(lhs), squashStrings(rhs))
    case Capture(e)               => Capture(squashStrings(e))
    case Optional(e)              => Optional(squashStrings(e))
    case ZeroOrMore(e)            => ZeroOrMore(squashStrings(e))
    case OneOrMore(e)             => OneOrMore(squashStrings(e))
    case Test(e)                  => Test(squashStrings(e))
    case Negate(e)                => Negate(squashStrings(e))

    case t => t
  }
}
