package optimizers

import sbtparboiled2boost.ParboiledParser._

object SquashStrings {
  def optimize(tree: Seq[Expr]): Seq[Expr] = tree.map {
    case RuleExpr(n, args, t, body) => RuleExpr(n, args, t, squashStrings(body))
    case t => ???
  }

  private def squashStrings(body: Expr): Expr = body match {
    case Sequence(exps) =>
      val squashed = exps.map(squashStrings).foldRight(Seq.empty[Expr]) {
        case (StringLiteral(s1), StringLiteral(s2) +: rest) => StringLiteral(s1 + s2) +: rest
        case (exp, acc) => exp +: acc
      }
      Sequence(squashed)

    case FirstOf(exps)            => FirstOf(exps.map(squashStrings))
    case Capture(e)               => Capture(squashStrings(e))
    case Optional(e)              => Optional(squashStrings(e))
    case ZeroOrMore(e)            => ZeroOrMore(squashStrings(e))
    case OneOrMore(e)             => OneOrMore(squashStrings(e))
    case Test(e)                  => Test(squashStrings(e))
    case Negate(e)                => Negate(squashStrings(e))

    case t => t
  }
}
