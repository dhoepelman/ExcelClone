package scalaExcel.model

import scalaExcel.formula.{VErr, CircularRef, VDouble, Value}
import scalaExcel.model.OperationHelpers._

import org.junit.Assert._
import org.junit._

class ModelTests {

  @Test def initializeModel() = {
    val model = new MutableModel()
    assertEquals(Map(), model.sheet.take(1).toBlocking.last.values)
  }

  @Test def changeFormula() = {
    val model = new MutableModel()
    val cell = model.sheet
      .filterCellValueAt((0, 0))
    model.changeFormula((0, 0), "=1+1")
    assertEquals(VDouble(2), cell.take(1).toBlocking.last)
  }

  @Test def formulaWithReferences() = {
    val model = new MutableModel()
    var y: Value = null
    model.sheet
      .filterCellValueAt((0, 2))
      .subscribe(x => y = x)

    model.changeFormula((0, 0), "=1+1")
    model.changeFormula((0, 1), "=A1 + 1")
    model.changeFormula((0, 2), "=A2 * 2")

    assertEquals(VDouble(6), y)
  }

  @Test def formulaUpdateDependents() = {
    val model = new MutableModel()
    var y: Value = null
    model.sheet
      .filterCellValueAt((0, 1))
      .subscribe(x => y = x)

    model.changeFormula((0, 0), "=1+1")
    model.changeFormula((0, 1), "=A1 + 1")
    model.changeFormula((0, 0), "=9")

    assertEquals(VDouble(10), y)
  }

  @Test def circularDependency1() = {
    val model = new MutableModel()
    var y: Value = null
    model.sheet
      .filterCellValueAt((2, 0))
      .subscribe(x => y = x)

    model.changeFormula((0, 0), "=C1")
    model.changeFormula((1, 0), "=5")
    model.changeFormula((2, 0), "=A1+B1")

    assertEquals(VErr(CircularRef), y)
  }

  @Test def circularDependency2() = {
    val model = new MutableModel()
    var y: Value = null
    model.sheet
      .filterCellValueAt((2, 0))
      .subscribe(x => y = x)

    model.changeFormula((0, 0), "=B1")
    model.changeFormula((1, 0), "=C1+1")
    model.changeFormula((2, 0), "=A1+1")

    assertEquals(VErr(CircularRef), y)
  }

  @Test def circularDependency3() = {
    val model = new MutableModel()
    var y: Value = null
    model.sheet
      .filterCellValueAt((0, 0))
      .subscribe(x => y = x)

    model.changeFormula((0, 0), "=5")
    model.changeFormula((1, 0), "=A1+5")
    model.changeFormula((2, 0), "=B1")
    model.changeFormula((0, 0), "=B1")

    assertEquals(VErr(CircularRef), y)
  }
}
