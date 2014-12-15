package scalaExcel.model.immutable

import org.junit.Assert._
import org.junit._

import scalaExcel.formula._
import scalaExcel.formula.Values.{toVal => tv}
import scalaExcel.model.immutable.Sorter.SheetSorter

class SorterTests {

  @Test def testDenseSingleColumnSort() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 1), "=4")
      .setCell((0, 2), "=2")
      .setCell((0, 3), "=3")
      .sort(0)

    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(2)), sheet.valueAt((0, 1)))
    assertEquals(Some(VDouble(3)), sheet.valueAt((0, 2)))
    assertEquals(Some(VDouble(4)), sheet.valueAt((0, 3)))
  }

  @Test def testSparseColumnSort() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 9), "=4")
      .setCell((0, 4), "=2")
      .setCell((0, 2), "=3")
      .sort(0)

    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(2)), sheet.valueAt((0, 1)))
    assertEquals(Some(VDouble(3)), sheet.valueAt((0, 2)))
    assertEquals(Some(VDouble(4)), sheet.valueAt((0, 3)))
    assertEquals(None, sheet.valueAt((0, 4)))
  }

  @Test def testColumnSortDesc() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 2), "=4")
      .setCell((0, 3), "=2")
      .setCell((0, 4), "=3")
      .sort(0, ascending = false)

    assertEquals(Some(VDouble(4)), sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(3)), sheet.valueAt((0, 1)))
    assertEquals(Some(VDouble(2)), sheet.valueAt((0, 2)))
    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 3)))
    assertEquals(None, sheet.valueAt((0, 4)))
  }

  @Test def testUpdateAllColumns() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1").setCell((1, 0), "=11")
      .setCell((0, 2), "=4").setCell((1, 2), "=12")
      .setCell((0, 4), "=2").setCell((1, 3), "=13")
      .setCell((0, 1), "=3").setCell((1, 1), "=14")
      .sort(0)

    assertEquals(Some(VDouble(1)),  sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(2)),  sheet.valueAt((0, 1)))
    assertEquals(Some(VDouble(3)),  sheet.valueAt((0, 2)))
    assertEquals(Some(VDouble(4)),  sheet.valueAt((0, 3)))

    assertEquals(Some(VDouble(11)), sheet.valueAt((1, 0)))
    assertEquals(None,              sheet.valueAt((1, 1)))
    assertEquals(Some(VDouble(14)), sheet.valueAt((1, 2)))
    assertEquals(Some(VDouble(12)), sheet.valueAt((1, 3)))
    assertEquals(Some(VDouble(13)), sheet.valueAt((1, 4)))
  }

  @Test def testUpdateDependents() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=3")
      .setCell((0, 1), "=A1-1")
      .setCell((0, 2), "=6+A1")
      .setCell((0, 3), "=8")
      .setCell((0, 4), "=A20")
      .sort(0)

    assertEquals(Some(VDouble(0)),     sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(2)),     sheet.valueAt((0, 1)))
    assertEquals(Some(VDouble(3)),     sheet.valueAt((0, 2)))
    assertEquals(Some(VDouble(8)),     sheet.valueAt((0, 3)))
    assertEquals(Some(VDouble(9)),     sheet.valueAt((0, 4)))
    assertEquals(List((0, 1), (0, 4)), sheet.dependents.get((0, 2)).get)
    assertEquals(List((0, 0)),         sheet.dependents.get((0, 19)).get)
    assertEquals("=A3 - 1",            sheet.getCell((0, 1)).f)
    assertEquals("=6 + A3",            sheet.getCell((0, 4)).f)
  }

}
