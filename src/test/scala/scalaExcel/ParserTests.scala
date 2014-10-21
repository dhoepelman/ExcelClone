
package scalaE

import org.junit.Test
import scalaExcel.Parser

class ParserTests {

  @Test def testArith {
    val parser = new Parser()
    println(parser.parseAll(parser.expr, "1 + 2"))
  }

}
