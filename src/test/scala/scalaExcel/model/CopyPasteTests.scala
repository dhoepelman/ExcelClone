package scalaExcel.model

import org.junit.Assert._
import org.junit.Test
import scalaExcel.model.OperationHelpers._

import scalaExcel.formula.{Value, VDouble}

class CopyPasteTests {

  @Test def copy1() = {
    val model = new Model()

    var y: Value = null
    model.sheet
      .filterCellValueAt((1, 1))
      .subscribe(x => y = x)

    model.changeFormula((0, 0), "1")
    model.changeFormula((1, 0), "2")
    model.changeFormula((0, 1), "=A1+1")

    model.copyCell((0,1),(1,1))

    assertEquals(VDouble(3), y)
  }

  @Test def cut1() = {
    val model = new Model()

    var y: Value = null
    model.sheet
      .filterCellValueAt((1, 1))
      .subscribe(x => y = x)

    model.changeFormula((0, 0), "1")
    model.changeFormula((1, 0), "2")
    model.changeFormula((0, 1), "=A1+1")

    model.cutCell((0,1),(1,1))

    assertEquals(VDouble(2), y)
  }

  @Test def cut2() = {
    val model = new Model()

    var sheet : Sheet = null
    model.sheet.subscribe(x => sheet = x)

    model.changeFormula((0, 0), "1")
    model.changeFormula((1, 0), "2")

    model.changeFormula((0, 1), "=A1+1")
    model.changeFormula((1, 1), "=$A$1+1")

    model.changeFormula((3, 0), "=C1+0.5")

    assertEquals(VDouble(0.5), sheet.valueAt((3,0)).get)

    model.cutCell((0,0), (2,0))

    assertEquals(VDouble(2), sheet.valueAt((0,1)).get)
    assertEquals(VDouble(2), sheet.valueAt((1,1)).get)
    assertEquals(VDouble(1.5), sheet.valueAt((3,0)).get)
  }
}
