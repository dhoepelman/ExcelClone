
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
  @Test def binOpSumString = teval(VErr(NotNumeric()), "=2 + \"a\"")
  @Test def binOpSumBool = teval(VErr(NotNumeric()), "=2 + TRUE")

  @Test def binOpMul = teval(VDouble(6), "=2 * 3")
  @Test def binOpMulString = teval(VErr(NotNumeric()), "=2 * \"a\"")
  @Test def binOpMulBool = teval(VErr(NotNumeric()), "=2 * TRUE")

  @Test def binOpConcat = teval(VString("abc"), "=\"ab\"& \"c\"")
  @Test def binOpConcatDouble = teval(VString("ab1"), "=\"ab\"& 1")
  @Test def binOpConcatTrue = teval(VString("abTRUE"), "=\"ab\"& TRUE")
  @Test def binOpConcatFalse = teval(VString("abFALSE"), "=\"ab\"& FALSE")

  @Test def unOpPlus          = teval(VDouble(5), "=+5")
  @Test def unOpPlusString    = teval(VErr(NotNumeric()), "=+\"A\"")
  @Test def unOpMin           = teval(VDouble(-5), "=-5")
  @Test def unOpMinString     = teval(VErr(NotNumeric()), "=-\"A\"")
  @Test def unOpPercent       = teval(VDouble(0.25), "=25%")
  @Test def unOpPercentString = teval(VErr(NotNumeric()), "=\"A\"%")

}

