
package scalaExcel

import org.junit.Assert._
import org.junit.Test

class NumberTests  {

  @Test def testNumberAdd {
    assertEquals(Number(15), Number(10).add(5))
  }

}
