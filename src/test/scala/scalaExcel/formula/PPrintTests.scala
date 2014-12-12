package scalaExcel.formula

import java.{util => ju, lang => jl}
import org.junit.Assert._
import org.junit._
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters

@RunWith(value = classOf[Parameterized])
class PPrintTests(f : String) {
  // Test if the formula equals its parsed and pretty-printed self
  @Test def test() = {
    val ast = Parser parsing f
    val pped = PPrinter.pprint(ast)
    //println(s"$f => $ast => $pped")
    assertEquals(f, pped)
  }
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
      "#REF!",
      "=(3 + 3) * 5",
      "=1 * 3 * 5",
      "=(5 * 5)",
      "=2 ^ (3 * 4)"
    ).foreach(n => data.add(Array(n)))
    data
  }
}
