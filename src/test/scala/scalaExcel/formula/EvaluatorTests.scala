
package scalaExcel.formula

import org.junit.Assert._
import org.junit._

import scalaExcel.formula.Evaluator.eval
import scalaExcel.formula.Values.{toVal => tv}

class EvaluatorTests {

  val p = new Parser()

  def teval(e: Value, v: Expr) = assertEquals(e, eval(v))
  def teval(e: Value, v: String) = assertEquals(e, eval(p parsing v))

  @Test def evalConst = teval(VDouble(10), Const(tv(10)))
  @Test def binOpSum = teval(VDouble(5), "=2 + 3")

}

