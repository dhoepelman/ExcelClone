package scalaExcel.model

import scalafx.scene.paint.Color
import rx.lang.scala.subjects.BehaviorSubject
import scalaExcel.model.Sorter._
import scalaExcel.model.Resizer._
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

  /** Perform a modification on the sheet */
  private def modifySheet(ur : UndoRedo[Sheet], action : ModelMutations) = {
    val sheet = ur.current
    action match {
      case SetFormula(pos, f) =>
        val (s, updates) = sheet.setCell(pos, f)
        ur.next(updateSheet(s, updates, Set(pos)))
      case EmptyCell(poss) => ur.next(
        poss.foldLeft(sheet)((sheet, pos) =>
          updateSheet(sheet.deleteCell(pos))
        )
      )
      case CopyCell(from, to) => ur.next(updateSheet(sheet.copyCell(from, to)))
      case CutCell(from, to) => ur.next(updateSheet(sheet.cutCell(from, to)))
      case SetColor(poss, c) => ur.next(
        poss.foldLeft(sheet)((sheet, pos) =>
          updateStyle(sheet, pos, s => s.setColor(c))
        )
      )
      case SetBackground(poss, c) => ur.next(
        poss.foldLeft(sheet)((sheet, pos) =>
          updateStyle(sheet, pos, s => s.setBackground(c))
        )
      )
      case SetSheet(values, styles) => ur.next(
        values.foldLeft(new Sheet(Map(), Map(), Map(), styles)) {
          case (acc, (pos, value)) =>
            val (s, updates) = acc.setCell(pos, value)
            updateSheet(s, updates, Set(pos))
          }
      )
      case SortColumn(x, asc) => ur.next(sheet.sort(x, asc))
      case AddColumns(count, index) => ur.next(sheet.addColumns(count, index))
      case AddRows(count, index) => ur.next(sheet.addRows(count, index))
      case RemoveColumns(count, index) => ur.next(sheet.removeColumns(count, index))
      case RemoveRows(count, index) => ur.next(sheet.removeRows(count, index))
      case ReorderColumns(permutations) =>
        ur.next(sheet.applyColumnPermutations(permutations))
      case Undo => ur.undo()
      case Redo => ur.redo()
      case Refresh => ur
    }
  }

  // this combines the initial Sheet with all input mutations from the outside
  // world
  private val undoRedoSheet =
    sheetMutations.scan(None: Option[Exception], new UndoRedo(new Sheet()))({
      case ((_, ur), action) =>
        try {
          (None, modifySheet(ur, action))
        } catch {
          case e: Exception =>
            // Do not modify the sheet
            (Some(e), ur)
        }
    })

  val errors = undoRedoSheet
    .filter({_._1.nonEmpty})
    .map({_._1.get})
  val sheet = undoRedoSheet
    .map({_._2.current})

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

  def addRows(count: Int, index: Int) = {
    sheetMutations.onNext(AddRows(count, index))
  }

  def addColumns(count: Int, index: Int) = {
    sheetMutations.onNext(AddColumns(count, index))
  }

  def removeRows(count: Int, index: Int) = {
    sheetMutations.onNext(RemoveRows(count, index))
  }

  def removeColumns(count: Int, index: Int) = {
    sheetMutations.onNext(RemoveColumns(count, index))
  }

  def reorderColumns(permutations: Map[Int, Int]) = {
    sheetMutations.onNext(ReorderColumns(permutations))
  }
}
