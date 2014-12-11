package scalaExcel.formula;

import org.junit.Assert._
import org.junit._

import scalaExcel.formula.Values.{toVal => tv}

class ValueTests {

  @Test def testCompare() = {
    // numbers should come first
    assertEquals(-1, tv(1).compare(tv(2)))
    assertEquals(1, tv(10).compare(tv(2)))
    assertEquals(-1, tv(1).compare(tv("a")))
    // then strings
    assertEquals(-1, tv("a").compare(tv("ab")))
    assertEquals(1, tv("b").compare(tv("a")))
    assertEquals(1, tv(true).compare(tv("ab")))
    // and finally booleans
    assertEquals(-1, tv(false).compare(tv(true)))
    assertEquals(0, tv(false).compare(tv(false)))
    assertEquals(0, tv(true).compare(tv(true)))
    assertEquals(1, tv(true).compare(tv(false)))
    // errors should come after other values
    assertEquals(-1, tv("a").compare(VErr(InvalidValue)))
    assertEquals(1, VErr(InvalidValue).compare(tv("a")))
    // Empty cells should come after everything else
    assertEquals(0, VEmpty.compare(VEmpty))
    assertEquals(1, VEmpty.compare(tv(1)))
    assertEquals(1, VEmpty.compare(tv(true)))
    assertEquals(1, VEmpty.compare(tv("ab")))
    assertEquals(-1, tv(1).compare(VEmpty))
    assertEquals(1, tv(true).compare(VEmpty))
    assertEquals(1, tv("ab").compare(VEmpty))
  }


}
