package scalaExcel.formula

import java.{util => ju, lang => jl}
import org.junit.Assert._
import org.junit._
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters

import scalaExcel.formula.Evaluator._

@RunWith(value = classOf[Parameterized])
class PPrintTests(f : String) {
  val p = new Parser()
  // Test if the formula equals its parsed and pretty-printed self
  @Test def test() = assertEquals(f, PPrinter.pprint(p parsing f))
}

object PPrintTests {
  @Parameters def parameters: ju.Collection[Array[jl.String]] = {
    val data = new ju.ArrayList[Array[jl.String]]()
    List(
      "0","1", "-5",
      "0.01", "1.03",
      "a", "abc",
      "'5", "'=1 +   1",
      "=1 + 1", "=5 / 8.9 + 9",
      "=3%", "=-3", "=+3",
      "=SUM(1,5,6,7,8)",
      "=A1", "=AZ55",
      "=$B$9", "=$B50",
      "=B15:C99", "=B$15:$C99",
      "=E:E",
      "=5:5",
      "=Sheet1!A5",
      "#REF!"
    ).foreach(n => data.add(Array(n)))
    data
  }
}