package scalaExcel.model

import scalafx.scene.paint.Color
import rx.lang.scala.subjects.BehaviorSubject
import scalaExcel.model.Sorter._
import scalaExcel.model.Slider._
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
    sheet.setCellStyle(pos, f(sheet.getCellStyle(pos)))

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
        case EmptyCell(poss) => ur.next(
          poss.foldLeft(sheet)( (sheet, pos) =>
            updateSheet(sheet.deleteCell(pos))
          )
        )
        case CopyCell(from, to) => ur.next(updateSheet(sheet.copyCell(from, to)))
        case CutCell(from, to) => ur.next(updateSheet(sheet.cutCell(from, to)))
        case SetColor(poss, c) => ur.next(
          poss.foldLeft(sheet)( (sheet, pos) =>
            updateStyle(sheet, pos, s => s.setColor(c))
          )
        )
        case SetBackground(poss, c) => ur.next(
          poss.foldLeft(sheet)( (sheet, pos) =>
            updateStyle(sheet, pos, s => s.setBackground(c))
          )
        )
        case SetSheet(values, styles) =>
          val styledSheet = new Sheet(Map(), Map(), Map(), styles)
          ur.next(values.foldLeft(styledSheet) { case (crtSheet, (pos, value)) =>
            val (s, updates) = sheet.setCell(pos, value)
            updateSheet(s, updates, Set(pos))
          })
        case SortColumn(x, asc) => ur.next(sheet.sort(x, asc))
        case Add(toRows, count, index) =>
          ur.next(updateSheet(sheet.slide(forward = true, toRows, count, index)))
        case Remove(fromRows, count, index) =>
          ur.next(updateSheet(sheet.slide(forward = false, fromRows, count, index)))
        case Undo => ur.undo()
        case Redo => ur.redo()
        case Refresh => ur
      }
  })

  val sheet = undoRedoSheet.map({a => a.current})

  def refresh() = sheetMutations.onNext(Refresh)

  def emptyCell(pos : CellPos) {
    emptyCell(List(pos))
  }

  def emptyCell(poss : Traversable[CellPos]) {
    sheetMutations.onNext(EmptyCell(poss))
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

  def changeBackground(pos : CellPos, c: Color) {
    changeBackground(List(pos), c)
  }

  def changeBackground(poss : Traversable[CellPos], c: Color) {
    sheetMutations.onNext(SetBackground(poss, c))
  }

  def changeColor(pos : CellPos, c: Color) {
    changeColor(List(pos),c)
  }

  def changeColor(poss : Traversable[CellPos], c: Color): Unit = {
    sheetMutations.onNext(SetColor(poss, c))
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

  def add(toRows: Boolean, count: Int, index: Int) = {
    sheetMutations.onNext(Add(toRows, count, index))
  }

  def remove(fromRows: Boolean, count: Int, index: Int) = {
    sheetMutations.onNext(Remove(fromRows, count, index))
  }
}
