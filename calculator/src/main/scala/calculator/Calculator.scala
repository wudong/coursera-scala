package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (name: String, y: Signal[Expr])=> {
        (name,Signal{eval(y(), namedExpressions)})
      }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def eval_detect_cycle(parentRef: Set[Ref], current: Expr): Double ={
      current match {
        case Literal(v) => v
        case Plus(a, b) => eval_detect_cycle(parentRef, a) + eval_detect_cycle(parentRef, b)
        case Minus(a, b) =>  eval_detect_cycle(parentRef, a) - eval_detect_cycle(parentRef, b)
        case Times(a, b) =>  eval_detect_cycle(parentRef, a) * eval_detect_cycle(parentRef, b)
        case Divide(a, b) =>  eval_detect_cycle(parentRef, a) / eval_detect_cycle(parentRef, b)
        case x: Ref =>  {
          if (parentRef.contains(x)) Double.NaN
          else {
            val newSet = parentRef + x
            val newExpr = getReferenceExpr(x.name, references)
            eval_detect_cycle(newSet, newExpr)
          }
        }
      }
    }

    eval_detect_cycle(Set(), expr)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
