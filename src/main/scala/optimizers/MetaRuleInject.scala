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
                case FirstOf(exps)                 => FirstOf(exps.map(substArgs))
                case Sequence(exps)                => Sequence(exps.map(substArgs))
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
        case FirstOf(exps)      => FirstOf(exps.map(substitute))
        case Sequence(exps)     => Sequence(exps.map(substitute))
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
      case RuleExpr(name, None, typ, body) =>
        def squashSeq(e: Expr): Expr = e match {
          case FirstOf(exps) =>
            val r = exps.map(squashSeq).foldRight(Seq.empty[Expr]) {
              case (FirstOf(es), rest) => es ++ rest
              case (exp, acc) => exp +: acc
            }
            FirstOf(r)
          case Sequence(exps) =>
            val r = exps.map(squashSeq).foldRight(Seq.empty[Expr]) {
              case (Sequence(es), rest) => es ++ rest
              case (exp, acc) => exp +: acc
            }
            Sequence(r)
          case Capture(e)         => Capture(squashSeq(e))
          case Optional(e)        => Optional(squashSeq(e))
          case ZeroOrMore(e)      => ZeroOrMore(squashSeq(e))
          case OneOrMore(e)       => OneOrMore(squashSeq(e))
          case Test(e)            => Test(squashSeq(e))
          case Negate(e)          => Negate(squashSeq(e))
          case t                  => t
        }
        RuleExpr(name, None, typ, squashSeq(substitute(body)))
      case _ => ???
    }
  }
}
