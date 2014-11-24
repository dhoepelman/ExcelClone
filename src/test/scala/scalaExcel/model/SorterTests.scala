package scalaExcel.model

import org.junit.Assert._
import org.junit._

import scalaExcel.formula._
import scalaExcel.formula.Values.{toVal => tv}
import scalaExcel.model.Sorter.{compare, SheetSorter}

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
    var sheet: Sheet = null

    model.sheet.subscribe(s => sheet = s)

    model.changeFormula(0, 0, "=1")
    model.changeFormula(0, 1, "=4")
    model.changeFormula(0, 2, "=2")
    model.changeFormula(0, 3, "=3")
    model.sheetMutations.onCompleted

    val newSheet = sheet.sort(0)
    assertEquals(Some(VDouble(1)), newSheet.valueAt(0, 0))
    assertEquals(Some(VDouble(2)), newSheet.valueAt(0, 1))
    assertEquals(Some(VDouble(3)), newSheet.valueAt(0, 2))
    assertEquals(Some(VDouble(4)), newSheet.valueAt(0, 3))
  }

  @Test def testParseSingleColumnSort() = {
    val model = new Model()
    var sheet: Sheet = null
    model.sheet.last.subscribe(s => sheet = s)

    model.changeFormula(0, 0, "=1")
    model.changeFormula(0, 2, "=4")
    model.changeFormula(0, 3, "=2")
    model.changeFormula(0, 4, "=3")
    model.sheetMutations.onCompleted

    val newSheet = sheet.sort(0)
    assertEquals(Some(VDouble(1)), newSheet.valueAt(0, 0))
    assertEquals(Some(VDouble(2)), newSheet.valueAt(0, 1))
    assertEquals(Some(VDouble(3)), newSheet.valueAt(0, 2))
    assertEquals(Some(VDouble(4)), newSheet.valueAt(0, 3))
    assertEquals(None, newSheet.valueAt(0, 4))
  }

  @Test def testUpdateAllColumns() = {
    val model = new Model()
    var sheet: Sheet = null
    model.sheet.last.subscribe(s => sheet = s)

    model.changeFormula(0, 0, "=1")
    model.changeFormula(0, 2, "=4")
    model.changeFormula(0, 3, "=2")
    model.changeFormula(0, 1, "=3")

    model.changeFormula(1, 0, "=1")
    model.changeFormula(1, 2, "=2")
    model.changeFormula(1, 3, "=3")
    model.changeFormula(1, 1, "=4")

    model.sheetMutations.onCompleted

    val newSheet = sheet.sort(0)

    assertEquals(Some(VDouble(1)), newSheet.valueAt(0, 0))
    assertEquals(Some(VDouble(2)), newSheet.valueAt(0, 1))
    assertEquals(Some(VDouble(3)), newSheet.valueAt(0, 2))
    assertEquals(Some(VDouble(4)), newSheet.valueAt(0, 3))

    assertEquals(Some(VDouble(1)), newSheet.valueAt(1, 0))
    assertEquals(Some(VDouble(3)), newSheet.valueAt(1, 1))
    assertEquals(Some(VDouble(4)), newSheet.valueAt(1, 2))
    assertEquals(Some(VDouble(2)), newSheet.valueAt(1, 3))
  }

  @Test def testUpdateDependents() = {
    val model = new Model()
    var sheet: Sheet = null
    model.sheet.last.subscribe(s => sheet = s)

    model.changeFormula(0, 0, "=2")
    model.changeFormula(0, 1, "=A1+4")
    model.changeFormula(0, 2, "=9")
    model.changeFormula(0, 3, "=8")

    model.sheetMutations.onCompleted

    val newSheet = sheet.sort(0)

    assertEquals(Some(VDouble(2)), newSheet.valueAt(0, 0))
    assertEquals(Some(VDouble(6)), newSheet.valueAt(0, 1))
    assertEquals(Some(VDouble(8)), newSheet.valueAt(0, 2))
    assertEquals(Some(VDouble(9)), newSheet.valueAt(0, 3))
  }

}
