package optimizers

import sbtparboiled2boost.ParboiledParser._

object MetaRuleInject {
  def optimize(tree: Seq[Expr]): Seq[Expr] = {
    val (metaRules, result) = tree.partition{
      case RuleExpr(_, Some(_), _, _) => true
      case RuleExpr(_, None, _, _)    => false
      case _ => ???
    }

    def substitute(body: Expr): Expr = {
      body match {
        case RuleCallExpr(name, args) =>
          metaRules.collectFirst { case re @ RuleExpr(nm, _, _, _) if nm == name => re } match {
            case Some(RuleExpr(_, metaArgs, _, metaBody)) =>
              val table = metaArgs.get.zip(args).toMap
              def substArgs(e: Expr): Expr = e match {
                case id: Identifier                => table.getOrElse(id, id)
                case FirstOf(lhs, rhs)             => FirstOf(substArgs(lhs), substArgs(rhs))
                case Sequence(lhs, rhs)            => Sequence(substArgs(lhs), substArgs(rhs))
                case Capture(x)                    => Capture(substArgs(x))
                case Optional(x)                   => Optional(substArgs(x))
                case ZeroOrMore(x)                 => ZeroOrMore(substArgs(x))
                case OneOrMore(x)                  => OneOrMore(substArgs(x))
                case sl: StringLiteral             => sl
                case Test(x)                       => Test(substArgs(x))
                case Negate(x)                     => Negate(substArgs(x))
                case _                             => ???
              }
              substArgs(metaBody)
            case None => ???
          }
        case FirstOf(lhs, rhs)  => FirstOf(substitute(lhs), substitute(rhs))
        case Sequence(lhs, rhs) => Sequence(substitute(lhs), substitute(rhs))
        case Capture(e)         => Capture(substitute(e))
        case Optional(e)        => Optional(substitute(e))
        case ZeroOrMore(e)      => ZeroOrMore(substitute(e))
        case OneOrMore(e)       => OneOrMore(substitute(e))
        case Test(e)            => Test(substitute(e))
        case Negate(e)          => Negate(substitute(e))
        case t                  => t
      }
    }

    result.map{
      case RuleExpr(name, None, typ, body) => RuleExpr(name, None, typ, substitute(body))
      case _ => ???
    }
  }
}
