package scalaExcel.model

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
