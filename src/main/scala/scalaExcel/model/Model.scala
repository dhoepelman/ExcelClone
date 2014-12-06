package scalaExcel.model

import scalaExcel.model.{ModelMutations, Sheet}
import scalafx.scene.paint.Color
import rx.lang.scala.Observable
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

  // this combines the initial Sheet with all input mutations from the outside
  // world
  private val undoRedoSheet = sheetMutations.scan(new UndoRedo(new Sheet()))({
    case (ur, action) =>
      val sheet = ur.current
      // Calculate new sheet
      action match {
        case SetFormula(pos, f) =>
          val (s, updates) = sheet.setCell(pos, f)
          ur.next(updateSheet(s, updates, Set(pos)))
        case EmptyCell(pos) => ur.next(updateSheet(sheet.deleteCell(pos)))
        case CopyCell(from, to) => ur.next(updateSheet(sheet.copyCell(from, to)))
        case CutCell(from, to) => ur.next(updateSheet(sheet.cutCell(from, to)))
        case SetColor(pos, c) => ur.next(updateStyle(sheet, pos, s => s.setColor(c)))
        case SetBackground(pos, c) => ur.next(updateStyle(sheet, pos, s => s.setBackground(c)))
        case SortColumn(x, asc) => ur.next(sheet.sort(x, asc))
        case Undo => ur.undo()
        case Redo => ur.redo()
        case Refresh => ur
      }
  })

  val sheet = undoRedoSheet.map({a => a.current})

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

  def undo() = {
    sheetMutations.onNext(Undo)
  }

  def redo() = {
    sheetMutations.onNext(Redo)
  }
}
