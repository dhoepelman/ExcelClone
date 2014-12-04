package scalaExcel.model

import scalafx.beans.value
import scalafx.scene.paint.Color
import rx.lang.scala.subjects.BehaviorSubject
import scalaExcel.model.Sorter.SheetSorter
import scalaExcel.model.OperationHelpers._
import scalaExcel.CellPos

class Model {
  /** This is a stream of inputs from 'the world' that will effect the state of the sheet model */
  val sheetMutations = BehaviorSubject[ModelMutations](Refresh)

  /**
   * function to propagate updates to dependent cells
   * @param alreadyUpdated Set of cells that were already updated, to detect cycles
   */
  def updateSheet(s: Sheet, updates: List[CellPos], alreadyUpdated: Set[CellPos] = Set()): Sheet = {
    updates.foldLeft(s)((s, u) => {
      if (alreadyUpdated contains u)
        // u was already updated, so this means there's a circular reference
        s.setToCircular(u)
      else
        s.updateCell(u) match {
          case (newSheet, List()) => newSheet
          case (newSheet, newUpdates) => updateSheet(newSheet, newUpdates, alreadyUpdated + u)
        }
    })
  }

  def updateSheet(x: (Sheet, List[(Int,Int)])): Sheet = x match {
    case (s, updates) => updateSheet(s, updates)
  }

  def updateStyle(sheet: Sheet, pos : CellPos, f: Styles => Styles) =
    sheet.setCellStyle(pos, f(sheet.styles.getOrElse(pos, Styles.DEFAULT)))

  private def setSheet(values: Traversable[((Int,Int),String)]): Sheet = {
    values.foldLeft(new Sheet()) { case (sheet, ((r, c), value)) =>
      val (s, updates) = sheet.setCell((c, r), value)
      s
    }
  }

  // this combines the initial Sheet with all input mutations from the outside
  // world
  val sheet = sheetMutations.scan(new Sheet())((sheet, action) => action match {
    case SetFormula(pos, f) =>
      val (s, updates) = sheet.setCell(pos, f)
      updateSheet(s, updates, Set(pos))
    case EmptyCell(pos) => updateSheet(sheet.deleteCell(pos))
    case CopyCell(from, to) => updateSheet(sheet.copyCell(from, to))
    case CutCell(from, to) => updateSheet(sheet.cutCell(from, to))
    case SetColor(pos, c) => updateStyle(sheet, pos, s => s.setColor(c))
    case SetBackground(pos, c) => updateStyle(sheet, pos, s => s.setBackground(c))
    case SetSheet(values, styles) => setSheet(values) // TODO also styles
    case SortColumn(x, asc) => sheet.sort(x, asc)
    case Refresh => sheet
  })

  def refresh() = sheetMutations.onNext(Refresh)

  def emptyCell(pos : CellPos)  {
    sheetMutations.onNext(EmptyCell(pos))
  }

  def copyCell(from : CellPos, to : CellPos) {
    sheetMutations.onNext(CopyCell(from, to))
  }

  def cutCell(from : CellPos, to : CellPos) {
    sheetMutations.onNext(CutCell(from, to))
  }

  def clearAndSet(values: Map[(Int,Int), String], styles: Map[(Int,Int), Styles]): Unit = {
    sheetMutations.onNext(SetSheet(values, styles))
  }

  def changeFormula(pos : CellPos, f: String) {
    sheetMutations.onNext(SetFormula(pos, f))
  }

  def changeBackground(pos : CellPos, c: Color): Unit = {
    sheetMutations.onNext(SetBackground(pos, c))
  }

  def changeColor(pos : CellPos, c: Color): Unit = {
    sheetMutations.onNext(SetColor(pos, c))
  }

  def sortColumn(x: Int, asc: Boolean) = {
    sheetMutations.onNext(SortColumn(x, asc))
  }

}
