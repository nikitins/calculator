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
    def fun(exprs: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
      if (exprs.isEmpty)
        Map()
      else {
        val name = exprs.head._1
        val expr = exprs.head._2
        if (check(expr(), namedExpressions, Set(name))) {
          fun(exprs.tail).updated(name, Signal(eval(expr(), namedExpressions)))
        } else {
          fun(exprs.tail).updated(name, Signal(Double.NaN))
        }
      }
    }
    fun(namedExpressions)
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(x) => x
    case Ref(name) => eval(getReferenceExpr(name, references), references)
    case Plus(a, b) => eval(a, references) + eval(b, references)
    case Minus(a, b) => eval(a, references) - eval(b, references)
    case Times(a, b) => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)
  }

  def check(expr: Expr, references: Map[String, Signal[Expr]], used: Set[String]): Boolean = expr match {
    case Literal(_) => true
    case Ref(name) => !used.contains(name) && check(getReferenceExpr(name, references), references, used + name)
    case x => fun(expr, references, used)
  }

  def fun(expr: Expr, references: Map[String, Signal[Expr]], used: Set[String]): Boolean = expr match {
    case Plus(a, b) => check(a, references, used) && check(b, references, used)
    case Minus(a, b) => check(a, references, used) && check(b, references, used)
    case Times(a, b) => check(a, references, used) && check(b, references, used)
    case Divide(a, b) => check(a, references, used) && check(b, references, used)
    case x => true
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
