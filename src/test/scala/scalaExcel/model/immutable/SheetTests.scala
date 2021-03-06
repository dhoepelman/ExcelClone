package scalaExcel.model.immutable

import org.junit.Assert._
import org.junit._

import scalafx.scene.paint.Color

import scalaExcel.formula._

class SheetTests {

  @Test def testEmptySheet() = {
    val sheet = new Sheet()
    assertEquals((0, 0), sheet.size)
  }

  @Test def setCell() = {
    val sheet = new Sheet().setCell((0, 0), "1")
    assertEquals((1, 1), sheet.size)
    assertEquals("1", sheet.getCell((0, 0)).f)
    assertEquals(VDouble(1), sheet.getValue((0, 0)))
  }

  @Test def setCellDependenciesSimple() = {
    val sheet = new Sheet()
      .setCell((0, 0), "0")
      .setCell((0, 1), "=A1")
      .setCell((0, 2), "=A1")

    val sheet1 = sheet
      .setCell((0, 0), "1")

    assertEquals(VDouble(1), sheet1.getValue((0, 0)))
    assertEquals(VDouble(1), sheet1.getValue((0, 1)))
    assertEquals(VDouble(1), sheet1.getValue((0, 2)))
  }

  @Test def setCellDependenciesFibonacci() = {
    val sheet = new Sheet()
      .setCell((0, 0), "0")
      .setCell((0, 1), "1")
      .setCell((0, 2), "=A1+A2")
      .setCell((0, 3), "=A2+A3")
      .setCell((0, 4), "=A3+A4")
      .setCell((0, 5), "=A4+A5")

    assertEquals(VDouble(0), sheet.getValue((0, 0)))
    assertEquals(VDouble(1), sheet.getValue((0, 1)))
    assertEquals(VDouble(1), sheet.getValue((0, 2)))
    assertEquals(VDouble(2), sheet.getValue((0, 3)))
    assertEquals(VDouble(3), sheet.getValue((0, 4)))
    assertEquals(VDouble(5), sheet.getValue((0, 5)))

    assertEquals(List((0, 2)),         sheet.dependentsOf((0, 0)))
    assertEquals(List((0, 2), (0, 3)), sheet.dependentsOf((0, 1)))
    assertEquals(List((0, 3), (0, 4)), sheet.dependentsOf((0, 2)))
    assertEquals(List((0, 4), (0, 5)), sheet.dependentsOf((0, 3)))
    assertEquals(List((0, 5)),         sheet.dependentsOf((0, 4)))
    assertEquals(List(),               sheet.dependentsOf((0, 5)))

    val sheet1 = sheet
      .setCell((0, 0), "1")

    assertEquals(VDouble(1), sheet1.getValue((0, 0)))
    assertEquals(VDouble(1), sheet1.getValue((0, 1)))
    assertEquals(VDouble(2), sheet1.getValue((0, 2)))
    assertEquals(VDouble(3), sheet1.getValue((0, 3)))
    assertEquals(VDouble(5), sheet1.getValue((0, 4)))
    assertEquals(VDouble(8), sheet1.getValue((0, 5)))
  }

  @Test def deleteCell() = {
    val sheet = new Sheet()
      .setCell((0, 0), "1")
      .deleteCell((0, 0))
    assertEquals((0, 0), sheet.size)
    assertEquals(VEmpty, sheet.getValue((0, 0)))
  }

  @Test def deleteCells() = {
    val sheet = new Sheet()
      .setCell((0, 0), "1")
      .setCell((1, 0), "1")
      .deleteCells(List((0, 0), (1, 0)))
    assertEquals((0, 0), sheet.size)
  }

  @Test def deleteCellsCleanupDependents() = {
    val sheet = new Sheet()
      .setCell((0, 0), "1")
      .setCell((0, 1), "=A1")
      .setCell((0, 2), "=A1")
      .deleteCell((0, 1))
    assertEquals((1, 3), sheet.size)
    assertEquals(Map((0, 0) -> List((0, 2))), sheet.dependents)

  }

  @Test def copyCell1() = {
    val sheet = new Sheet()
      .setCell((0, 0), "1")
      .setCell((1, 0), "2")
      .setCell((0, 1), "=A1+1")
      .copyCell((0, 1), (1, 1))
    assertEquals((2, 2), sheet.size)
    assertEquals(VDouble(1), sheet.getValue((0, 0)))
    assertEquals(VDouble(2), sheet.getValue((1, 0)))
    assertEquals(VDouble(3), sheet.getValue((1, 1)))
  }

  @Test def cutCell1() = {
    val sheet = new Sheet()
      .setCell((0, 0), "1")
      .setCell((1, 0), "2")
      .setCell((0, 1), "=A1+1")
      .cutCell((0,1),(1,1))
    assertEquals(VDouble(2), sheet.getValue((1, 1)))
  }

  @Test def cutCell2() = {
    val sheet = new Sheet()
      .setCell((0, 0), "1")
      .setCell((1, 0), "2")
      .setCell((0, 1), "=A1+1")
      .setCell((1, 1), "=$A$1+1")
      .setCell((3, 0), "=C1+0.5")
    assertEquals(VDouble(0.5), sheet.getValue((3, 0)))

    val sheet2 = sheet.cutCell((0, 0), (2, 0))

    assertEquals(VDouble(2), sheet2.getValue((0, 1)))
    assertEquals(VDouble(2), sheet2.getValue((1, 1)))
    assertEquals(VDouble(1.5), sheet2.getValue((3, 0)))
  }

  @Test def updateStyle() = {
    val sheet = new Sheet()
      .updateCellStyle((0, 0), s => s.setBackground(Color.Black))
    assertEquals(Color.Black, sheet.getCellStyle((0, 0)).background)
  }

  @Test def circularDependency1() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=C1")
      .setCell((1, 0), "=5")
      .setCell((2, 0), "=A1+B1")
    assertEquals(VErr(CircularRef), sheet.getValue((2, 0)))
  }

  @Test def circularDependency2() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=B1")
      .setCell((1, 0), "=C1+1")
      .setCell((2, 0), "=A1+1")
    assertEquals(VErr(CircularRef), sheet.getValue((2, 0)))
  }

  @Test def circularDependency3() = {
    val sheet = new Sheet()
      .setCell((0, 0), "=5")
      .setCell((1, 0), "=A1+5")
      .setCell((2, 0), "=B1")
      .setCell((0, 0), "=B1")
    assertEquals(VErr(CircularRef), sheet.getValue((0, 0)))
  }


}
