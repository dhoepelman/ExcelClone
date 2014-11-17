package scalaExcel.model

import scalaExcel.formula.{VDouble, Value}
import scalaExcel.model.OperationHelpers._

import org.junit.Assert._
import org.junit._

class ModelTests {

  @Test def initializeModel() = {
    val model = new Model()
    var sheet: Sheet = null
    model.sheet.take(1).subscribe(x => {
      sheet = x
    })
    model.refresh()
    assertEquals(Map(), sheet.values)
  }

  @Test def changeFormula() = {
    val model = new Model()
    var y: Value = null
    model.sheet
      .filterCellValueAt(1, 1)
      .subscribe(x => y = x)

    model.changeFormula(1, 1, "=1+1")

    assertEquals(VDouble(2), y)
  }

  @Test def formulaWithReferences() = {
    val model = new Model()
    var y: Value = null
    model.sheet
      .filterCellValueAt(1, 3)
      .subscribe(x => y = x)

    model.changeFormula(1, 1, "=1+1")
    model.changeFormula(1, 2, "=A1 + 1")
    model.changeFormula(1, 3, "=A2 * 2")

    assertEquals(VDouble(6), y)
  }

  @Test def formulaUpdateDependents() = {
    val model = new Model()
    var y: Value = null
    model.sheet
      .filterCellValueAt(1, 2)
      .subscribe(x => y = x)

    model.changeFormula(1, 1, "=1+1")
    model.changeFormula(1, 2, "=A1 + 1")
    model.changeFormula(1, 1, "=9")

    assertEquals(VDouble(10), y)
  }

}
