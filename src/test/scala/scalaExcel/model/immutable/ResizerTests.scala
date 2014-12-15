package scalaExcel.model.immutable

import org.junit.Test
import org.junit.Assert._
import scalaExcel.formula.VDouble
import scalaExcel.model.immutable.Resizer.SheetResizer

class ResizerTests {

  @Test def testSingleRowRemove() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 1), "=2")
      .setCell((0, 2), "=3")
      .removeRows(1, 1)

    assertEquals((1, 2), sheet.size)
    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(3)), sheet.valueAt((0, 1)))
  }

  @Test def testMultiRowRemove() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 1), "=2")
      .setCell((0, 2), "=3")
      .setCell((0, 3), "=4")
      .removeRows(2, 1)

    assertEquals((1, 2), sheet.size)
    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(4)), sheet.valueAt((0, 1)))
  }

  @Test def testAllRowsRemove() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 1), "=2")
      .setCell((0, 2), "=3")
      .setCell((0, 3), "=4")
      .removeRows(4, 0)

    assertEquals((0, 0), sheet.size)
  }

  @Test def testSingleColumnRemove() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((1, 0), "=2")
      .setCell((2, 0), "=3")
      .removeColumns(1, 1)

    assertEquals((2, 1), sheet.size)
    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(3)), sheet.valueAt((1, 0)))
  }

  @Test def testMultiColumnRemove() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((1, 0), "=2")
      .setCell((2, 0), "=3")
      .setCell((3, 0), "=4")
      .removeColumns(2, 1)

    assertEquals((2, 1), sheet.size)
    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(4)), sheet.valueAt((1, 0)))
  }

  @Test def testAllColumnsRemove() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((1, 0), "=2")
      .setCell((2, 0), "=3")
      .setCell((3, 0), "=4")
      .removeColumns(4, 0)

    assertEquals((0, 0), sheet.size)
  }

  @Test def testSparseRowColumnsRemove() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 2), "=2")
      .setCell((0, 3), "=3")
      .setCell((1, 1), "=4")
      .setCell((1, 2), "=5")
      .setCell((1, 4), "=6")
      .removeRows(1, 0)
      .removeColumns(1, 0)

    assertEquals((1, 4), sheet.size)
    assertEquals(Some(VDouble(4)), sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(5)), sheet.valueAt((0, 1)))
    assertEquals(None, sheet.valueAt((0, 2)))
    assertEquals(Some(VDouble(6)), sheet.valueAt((0, 3)))
  }

  @Test def testFirstRowAdd() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 1), "=2")
      .setCell((0, 2), "=3")
      .addRows(1, 0)

    assertEquals((1, 4), sheet.size)
    assertEquals(None, sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 1)))
    assertEquals(Some(VDouble(2)), sheet.valueAt((0, 2)))
    assertEquals(Some(VDouble(3)), sheet.valueAt((0, 3)))
  }

  @Test def testMultiRowAdd() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 1), "=2")
      .setCell((0, 2), "=3")
      .addRows(2, 1)

    assertEquals((1, 5), sheet.size)
    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 0)))
    assertEquals(None, sheet.valueAt((0, 1)))
    assertEquals(None, sheet.valueAt((0, 2)))
    assertEquals(Some(VDouble(2)), sheet.valueAt((0, 3)))
    assertEquals(Some(VDouble(3)), sheet.valueAt((0, 4)))
  }

  @Test def testAfterRowsAdd() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 1), "=2")
      .addRows(4, 2)

    assertEquals((1, 2), sheet.size)
    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(2)), sheet.valueAt((0, 1)))
  }

  @Test def testFirstColumnAdd() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((1, 0), "=2")
      .setCell((2, 0), "=3")
      .addColumns(1, 0)

    assertEquals((4, 1), sheet.size)
    assertEquals(None, sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(1)), sheet.valueAt((1, 0)))
    assertEquals(Some(VDouble(2)), sheet.valueAt((2, 0)))
    assertEquals(Some(VDouble(3)), sheet.valueAt((3, 0)))
  }

  @Test def testMultiColumnAdd() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((1, 0), "=2")
      .setCell((2, 0), "=3")
      .addColumns(2, 1)

    assertEquals((5, 1), sheet.size)
    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 0)))
    assertEquals(None, sheet.valueAt((1, 0)))
    assertEquals(None, sheet.valueAt((2, 0)))
    assertEquals(Some(VDouble(2)), sheet.valueAt((3, 0)))
    assertEquals(Some(VDouble(3)), sheet.valueAt((4, 0)))
  }

  @Test def testAfterColumnsAdd() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((1, 0), "=2")
      .addColumns(4, 2)

    assertEquals((2, 1), sheet.size)
    assertEquals(Some(VDouble(1)), sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(2)), sheet.valueAt((1, 0)))
  }

  @Test def testSparseRowColumnsAdd() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=1")
      .setCell((0, 2), "=2")
      .setCell((0, 3), "=3")
      .setCell((1, 1), "=4")
      .setCell((1, 2), "=5")
      .setCell((1, 4), "=6")
      .addRows(1, 0)
      .addColumns(1, 0)

    assertEquals((3, 6), sheet.size)
    assertEquals(None, sheet.valueAt((0, 0)))
    assertEquals(None, sheet.valueAt((0, 1)))
    assertEquals(None, sheet.valueAt((0, 2)))
    assertEquals(None, sheet.valueAt((0, 3)))
    assertEquals(None, sheet.valueAt((0, 4)))
    assertEquals(None, sheet.valueAt((0, 5)))
    assertEquals(None, sheet.valueAt((1, 0)))
    assertEquals(Some(VDouble(1)), sheet.valueAt((1, 1)))
    assertEquals(None, sheet.valueAt((1, 2)))
    assertEquals(Some(VDouble(2)), sheet.valueAt((1, 3)))
    assertEquals(Some(VDouble(3)), sheet.valueAt((1, 4)))
    assertEquals(None, sheet.valueAt((1, 5)))
    assertEquals(None, sheet.valueAt((2, 0)))
    assertEquals(None, sheet.valueAt((2, 1)))
    assertEquals(Some(VDouble(4)), sheet.valueAt((2, 2)))
    assertEquals(Some(VDouble(5)), sheet.valueAt((2, 3)))
    assertEquals(None, sheet.valueAt((2, 4)))
    assertEquals(Some(VDouble(6)), sheet.valueAt((2, 5)))
  }

  @Test def testUpdateValidDependentsOnRemoveRow() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=6")
      .setCell((0, 1), "=A5")
      .setCell((0, 2), "=A5-1")
      .setCell((0, 3), "=6+A3")
      .setCell((0, 4), "=A1")
      .removeRows(1, 1)

    assertEquals((1, 4), sheet.size)
    assertEquals(Some(VDouble(6)),     sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(5)),     sheet.valueAt((0, 1)))
    assertEquals(Some(VDouble(11)),    sheet.valueAt((0, 2)))
    assertEquals(Some(VDouble(6)),     sheet.valueAt((0, 3)))
    assertEquals(List((0, 3)),         sheet.dependents.get((0, 0)).get)
    assertEquals(List((0, 2)),         sheet.dependents.get((0, 1)).get)
    // TODO add again when delete bug is fixed
//    assertEquals(List((0, 1)),         sheet.dependents.get((0, 3)).get)
    assertEquals("=A4 - 1",            sheet.getCell((0, 1)).f)
    assertEquals("=6 + A2",            sheet.getCell((0, 2)).f)
    assertEquals("=A1",                sheet.getCell((0, 3)).f)
  }

  @Test def testUpdateValidDependentsOnRemoveColumn() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=6")
      .setCell((1, 0), "=E1")
      .setCell((2, 0), "=E1-1")
      .setCell((3, 0), "=6+C1")
      .setCell((4, 0), "=A1")
      .removeColumns(1, 1)

    assertEquals((4, 1), sheet.size)
    assertEquals(Some(VDouble(6)),     sheet.valueAt((0, 0)))
    assertEquals(Some(VDouble(5)),     sheet.valueAt((1, 0)))
    assertEquals(Some(VDouble(11)),    sheet.valueAt((2, 0)))
    assertEquals(Some(VDouble(6)),     sheet.valueAt((3, 0)))
    assertEquals(List((3, 0)),         sheet.dependents.get((0, 0)).get)
    assertEquals(List((2, 0)),         sheet.dependents.get((1, 0)).get)
    // TODO add again when delete bug is fixed
//    assertEquals(List((1, 0)),         sheet.dependents.get((3, 0)).get)
    assertEquals("=D1 - 1",            sheet.getCell((1, 0)).f)
    assertEquals("=6 + B1",            sheet.getCell((2, 0)).f)
    assertEquals("=A1",                sheet.getCell((3, 0)).f)
  }

  @Test def testUpdateValidDependentsOnAddRow() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=6")
      .setCell((0, 1), "=A4-1")
      .setCell((0, 2), "=6+A2")
      .setCell((0, 3), "=A1")
      .addRows(1, 1)

    assertEquals((1, 5), sheet.size)
    assertEquals(Some(VDouble(6)),     sheet.valueAt((0, 0)))
    assertEquals(None,                 sheet.valueAt((0, 1)))
    assertEquals(Some(VDouble(5)),     sheet.valueAt((0, 2)))
    assertEquals(Some(VDouble(11)),    sheet.valueAt((0, 3)))
    assertEquals(Some(VDouble(6)),     sheet.valueAt((0, 4)))
    assertEquals(List((0, 4)),         sheet.dependents.get((0, 0)).get)
    assertEquals(None,                 sheet.dependents.get((0, 1)))
    assertEquals(List((0, 3)),         sheet.dependents.get((0, 2)).get)
    assertEquals(List((0, 2)),         sheet.dependents.get((0, 4)).get)
    assertEquals("=A5 - 1",            sheet.getCell((0, 2)).f)
    assertEquals("=6 + A3",            sheet.getCell((0, 3)).f)
    assertEquals("=A1",                sheet.getCell((0, 4)).f)
  }

  @Test def testUpdateValidDependentsOnAddColumn() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=6")
      .setCell((1, 0), "=D1-1")
      .setCell((2, 0), "=6+B1")
      .setCell((3, 0), "=A1")
      .addColumns(1, 1)

    assertEquals((5, 1), sheet.size)
    assertEquals(Some(VDouble(6)),     sheet.valueAt((0, 0)))
    assertEquals(None,                 sheet.valueAt((1, 0)))
    assertEquals(Some(VDouble(5)),     sheet.valueAt((2, 0)))
    assertEquals(Some(VDouble(11)),    sheet.valueAt((3, 0)))
    assertEquals(Some(VDouble(6)),     sheet.valueAt((4, 0)))
    assertEquals(List((4, 0)),         sheet.dependents.get((0, 0)).get)
    assertEquals(None,                 sheet.dependents.get((1, 0)))
    assertEquals(List((3, 0)),         sheet.dependents.get((2, 0)).get)
    assertEquals(List((2, 0)),         sheet.dependents.get((4, 0)).get)
    assertEquals("=E1 - 1",            sheet.getCell((2, 0)).f)
    assertEquals("=6 + C1",            sheet.getCell((3, 0)).f)
    assertEquals("=A1",                sheet.getCell((4, 0)).f)
  }

  //TODO implement once row references get invalidated on remove
  @Test def testInvalidateDependentsOnRemoveRow() = {

  }

  //TODO implement once column references get invalidated on remove
  @Test def testInvalidateDependentsOnRemoveColumn() = {

  }
}
