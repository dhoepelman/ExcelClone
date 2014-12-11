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

  @Test def copyCell() = {
    val sheet = new Sheet()
      .setCell((0, 0), "1")
      .copyCell((0, 0), (1, 0))
    assertEquals((2, 1), sheet.size)
    assertEquals(VDouble(1), sheet.getValue((0, 0)))
    assertEquals(VDouble(1), sheet.getValue((1, 0)))
  }

  @Test def updateStyle() = {
    val sheet = new Sheet()
      .updateCellStyle((0, 0), s => s.setBackground(Color.Black))
    assertEquals(Color.Black, sheet.getCellStyle((0, 0)).background)
  }

}
