package scalaExcel.model.immutable

import org.junit.Assert._
import org.junit._
import rx.lang.scala.Observable

import scalaExcel.formula.VDouble
import scalaExcel.model._
import scalaExcel.model.immutable.OperationHelpers._

class ModelTests {

  @Test def initializeModel() = {
    val model = new Model(
      Observable.just(Refresh)
    )
    assertEquals(Map(), model.sheet.take(1).toBlocking.last.values)
  }

  @Test def changeFormula() = {
    val model = new Model(
      Observable.just(SetFormula((0,0), "=1+1"))
    )
    val cell = model.sheet
      .filterCellValueAt((0, 0))
    assertEquals(VDouble(2), cell.take(1).toBlocking.last)
  }

  @Test def formulaWithReferences() = {
    val model = new Model(Observable.from(List(
      SetFormula((0, 0), "=1+1"),
      SetFormula((0, 1), "=A1 + 1"),
      SetFormula((0, 2), "=A2 * 2")
    )))

    val cell = model.sheet
      .filterCellValueAt((0, 2))

    assertEquals(VDouble(6), cell.toBlocking.last)
  }

  @Test def formulaUpdateDependents() = {
    val model = new Model(Observable.from(List(
      SetFormula((0, 0), "=1+1"),
      SetFormula((0, 1), "=A1 + 1"),
      SetFormula((0, 0), "=9")
    )))

    val cell = model.sheet
      .filterCellValueAt((0, 1))

    assertEquals(VDouble(10), cell.toBlocking.last)
  }

}
