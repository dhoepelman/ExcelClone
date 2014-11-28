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
      .filterCellValueAt(2, 2)
      .subscribe(x => y = x)

    model.changeFormula(1, 1, "1")
    model.changeFormula(2, 1, "2")
    model.changeFormula(1, 2, "=A1+1")

    model.copyCell((1,2),(2,2))

    assertEquals(VDouble(3), y)
  }

  @Test def cut1() = {
    val model = new Model()

    var y: Value = null
    model.sheet
      .filterCellValueAt(2, 2)
      .subscribe(x => y = x)

    model.changeFormula(1, 1, "1")
    model.changeFormula(2, 1, "2")
    model.changeFormula(1, 2, "=A1+1")

    model.cutCell((1,2),(2,2))

    assertEquals(VDouble(2), y)
  }

  @Test def cut2() = {
    val model = new Model()

    var sheet : Sheet = null
    model.sheet.subscribe(x => sheet = x)

    model.changeFormula(1, 1, "1")
    model.changeFormula(2, 1, "2")

    model.changeFormula(1, 2, "=A1+1")
    model.changeFormula(2, 2, "=$A$1+1")

    model.changeFormula(4, 1, "=C1+0.5")

    assertEquals(VDouble(0.5), sheet.valueAt(4,1).get)

    model.cutCell((1,1), (3,1))

    assertEquals(VDouble(2), sheet.valueAt(1,2).get)
    assertEquals(VDouble(2), sheet.valueAt(2,2).get)
    assertEquals(VDouble(1.5), sheet.valueAt(4,1).get)
  }
}
