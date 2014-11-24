package scalaExcel.model

import scalafx.scene.paint.Color
import rx.lang.scala.subjects.BehaviorSubject
import scalaExcel.model.OperationHelpers._

class Model {

  /** This is a stream of inputs from 'the world' that will effect the state of the sheet model */
  val sheetMutations = BehaviorSubject.apply[ModelMutations](Refresh())

  /**
   * function to propagate updates to dependent cells
   * @param alreadyUpdated Set of cells that were already updated, to detect cycles
   */
  def updateSheet(s: Sheet, updates: List[(Int,Int)], alreadyUpdated: Set[(Int, Int)] = Set()): Sheet = {
    updates.foldLeft(s)((s, u) => {
      if (alreadyUpdated contains u)
        // u was already updated, so this means there's a circular reference
        s.setToCircular(u._1, u._2)
      else
        s.updateCell(u._1, u._2) match {
          case (newSheet, List()) => newSheet
          case (newSheet, newUpdates) => updateSheet(newSheet, newUpdates, alreadyUpdated + u)
        }
    })
  }

  def updateSheet(x: (Sheet, List[(Int,Int)])): Sheet = x match {
    case (s, updates) => updateSheet(s, updates)
  }

  // this combines the initial Sheet with all input mutations from the outside
  // world
  val sheet = sheetMutations.scan(new Sheet())((sheet, action) => action match {
    case SetFormula(x, y, f) => {
      val (s, updates) = sheet.setCell(x, y, f)
      updateSheet(s, updates, Set((x, y)))
    }
    //case SetColor(x, y, c) => sheet.setCellColor(x, y, c)
    case SetStyle(x, y, s) => sheet.setCellStyle(x, y, s)
    case Refresh() => sheet
  })

  def refresh() = sheetMutations.onNext(Refresh())

  def changeFormula(x: Int, y: Int, f: String) {
    sheetMutations.onNext(SetFormula(x, y, f))
  }

//  def changeColor(x: Int, y: Int, c: Color) {
//    sheetMutations.onNext(SetColor(x, y, c))
//  }

  def changeStyle(x: Int, y: Int, s: Styles): Unit = {
    sheetMutations.onNext(SetStyle(x, y, s))
  }

}

object ModelExample extends App {
  val model = new Model()

  // Anything can subscribe to this stream of
  model.sheet.subscribe(x => println(x.values))

  // Or to just get the distinct values at cell (3, 1)
  model.sheet
    .map(_.valueAt(3, 1))
    .filter(!_.isEmpty)
    .map(_.get)
    .distinctUntilChanged
    .subscribe(x => println(s"value at (3,1) changed to $x"))

  // Or use the implicit helper class and use the this:
  model.sheet.filterCellValueAt(4, 1).subscribe(x => println(s"(4,1) value changed to $x"))

  model.sheet
    .map(s => s.styles)
    .distinctUntilChanged
    .subscribe(x => println(s"styles $x"))

  // Input some changes
  model.changeFormula(1, 1, "=C1")
  model.changeFormula(2, 1, "=5")
  model.changeFormula(3, 1, "=A1+B1")
  model.changeFormula(4, 1, "=A1+A1")
  model.changeFormula(1, 1, "=4+6")
  model.changeFormula(2, 1, "=4+0")

//  model.changeColor(1, 1, Color.Yellow)
}
