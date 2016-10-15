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
    namedExpressions map {
      case (key, expr) => key -> Var(eval(expr(), namedExpressions))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case value: Literal => value.v
      case plus: Plus => eval(plus.a, references) + eval(plus.b, references)
      case minus: Minus => eval(minus.a, references) - eval(minus.b, references)
      case times: Times => eval(times.a, references) * eval(times.b, references)
      case divide: Divide => eval(divide.a, references) / eval(divide.b, references)
      case ref: Ref =>
        if (!references.contains(ref.name)) Double.NaN else {
          val expr = references(ref.name)
          val newReferences = references - ref.name
          eval(expr(), newReferences)
        }
    }
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
