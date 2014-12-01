package scalaExcel.formula

import org.junit.Assert._
import org.junit.Test

class PackageTest {
  // test column name utils
  @Test def colToNum1() = assertEquals(0,   colToNum("A"))
  @Test def colToNum2() = assertEquals(26,  colToNum("AA"))
  @Test def colToNum3() = assertEquals(31,  colToNum("AF"))
  @Test def colToNum4() = assertEquals(702, colToNum("AAA"))

  @Test def numToCol1() = assertEquals("A",   numToCol(0))
  @Test def numToCol2() = assertEquals("AA",  numToCol(26))
  @Test def numToCol3() = assertEquals("AF",  numToCol(31))
  @Test def numToCol4() = assertEquals("AAA", numToCol(702))
}
