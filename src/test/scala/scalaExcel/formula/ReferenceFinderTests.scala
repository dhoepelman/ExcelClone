package scalaExcel.formula

import org.junit.Assert._
import org.junit._

import scalaExcel.formula.ReferenceFinder.find
import scalaExcel.formula.Values.{toVal => tv}

class ReferenceFinderTests {

  val p = new Parser

  def test(expect: List[Expr], f: String) = assertEquals(
    expect,
    find(p parsing(f))
  )

  def ltv(l: List[Any]) = l map (x => Const(tv(x)))

  @Test def findBinOp1 = test(ltv(List(1, 2)), "=1+2")
  @Test def findBinOp2 = test(ltv(List(1, 2, 3)), "=1+2+3")
  @Test def findBinOp3 = test(ltv(List(1, 2, 3, 4)), "=(1+2) + SUM(3, 4%)")

}
