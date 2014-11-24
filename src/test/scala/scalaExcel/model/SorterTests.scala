package scalaExcel.model

import org.junit.Assert._
import org.junit._

import scalaExcel.formula._
import scalaExcel.formula.Values.{toVal => tv}
import scalaExcel.model.Sorter.compare

class SorterTests {

  @Test def testCompare() = {
    // numbers should come first
    assertTrue(compare(tv(1), tv(2)))
    assertFalse(compare(tv(10), tv(2)))
    assertTrue(compare(tv(1), tv("a")))
    // then strings
    assertTrue(compare(tv("a"), tv("ab")))
    assertFalse(compare(tv("b"), tv("a")))
    assertFalse(compare(tv(true), tv("ab")))
    // and finally booleans
    assertTrue(compare(tv(false), tv(true)))
    assertTrue(compare(tv(false), tv(false)))
    assertTrue(compare(tv(true), tv(true)))
    assertTrue(compare(tv(false), tv(false)))
    assertFalse(compare(tv(true), tv(false)))
    // errors should come last
    assertTrue(compare(tv("a"), VErr(InvalidValue)))
    assertFalse(compare(VErr(InvalidValue), tv("a")))
  }

  @Test def testDenseSingleColumnSort() = {
    val model = new Model()
    var tested = false
    model.sheet.last.subscribe(sheet => {
      val newSheet = Sorter.sort(sheet, 0)
      assertEquals(Some(VDouble(1)), newSheet.valueAt(0, 0))
      assertEquals(Some(VDouble(2)), newSheet.valueAt(0, 1))
      assertEquals(Some(VDouble(3)), newSheet.valueAt(0, 2))
      assertEquals(Some(VDouble(4)), newSheet.valueAt(0, 3))
      tested = true
    })

    model.changeFormula(0, 0, "=1")
    model.changeFormula(0, 1, "=4")
    model.changeFormula(0, 2, "=2")
    model.changeFormula(0, 3, "=3")
    model.sheetMutations.onCompleted

    assertTrue(tested)
  }

}
