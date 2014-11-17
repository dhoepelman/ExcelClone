package scalaExcel.model

import rx.lang.scala.{Observable, Observer, Subject}

import scalaExcel.formula.Value

class Model {

  // This is a stream of inputs from 'the world' that will effect the state of
  // the sheet model
  val sheetMutations = Subject[SheetMutations]()

  // function to propagate updates to dependent cells
  def updateSheet(s: Sheet, updates: List[(Int, Int)]): Sheet = {
    updates.foldLeft(s)((s, u) => s.updateCell(u._1, u._2) match {
      case (newSheet, List()) => newSheet
      case (newSheet, newUpdates) => updateSheet(newSheet, newUpdates)
    })
  }

  def updateSheet(x: (Sheet, List[(Int,Int)])): Sheet = x match {
    case (s, updates) => updateSheet(s, updates)
  }

  // this combines the initial Sheet with all input mutations from the outside
  // world
  val sheet = sheetMutations.scan(new Sheet())((sheet, action) => action match {
    case SetCell(x, y, f) => updateSheet(sheet.setCell(x, y, f))
  })

  def changeFormula(x: Int, y: Int , f: String) {
    sheetMutations.onNext(SetCell(x, y, f))
  }
}

object ModelTest extends App {
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

  // Or we can define Observable[Sheet] method extensions
  implicit class ExtendedObservableSheet(val sheet: Observable[Sheet]) extends AnyVal {
    def filterCellValueAt(x: Int, y: Int) =
      sheet
        .map(_.valueAt(x, y))
        .filter(!_.isEmpty)
        .map(_.get)
        .distinctUntilChanged
  }

  // And use the this:
  model.sheet.filterCellValueAt(4, 1).subscribe(x => println(s"(4,1) value changed to $x"))

  // Input some changes
  model.changeFormula(1, 1, "=1+2")
  model.changeFormula(2, 1, "=A1+A1")
  model.changeFormula(3, 1, "=A1+B1")
  model.changeFormula(4, 1, "=A1+A1")
  model.changeFormula(1, 1, "=4+6")
  model.changeFormula(2, 1, "=4+0")
}