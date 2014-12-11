package scalaExcel.model

import scalafx.scene.paint.Color
import rx.lang.scala.Observable
import rx.lang.scala.subjects.BehaviorSubject
import scalaExcel.model.Sorter.SheetSorter
import scalaExcel.model.OperationHelpers._
import scalaExcel.CellPos

/**
 * Represents the data model of the ScalaExcel application
 * @param sheetMutations Inputs from outside this package that will effect the state of the sheet model
 */
class Model(protected val sheetMutations : Observable[ModelMutations]) {
  /**
   * function to propagate updates to dependent cells
   * @param alreadyUpdated Set of cells that were already updated, to detect cycles
   */
  private def updateSheet(s: Sheet, updates: List[CellPos], alreadyUpdated: Set[CellPos] = Set()): Sheet = {
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

  private def updateSheet(x: (Sheet, List[(Int,Int)])): Sheet = x match {
    case (s, updates) => updateSheet(s, updates)
  }

  private def updateStyle(sheet: Sheet, pos : CellPos, f: Styles => Styles) =
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
          case (sheet, (pos, value)) =>
            val (s, updates) = sheet.setCell(pos, value)
            updateSheet(s, updates, Set(pos))
        }
      )
      case SortColumn(x, asc) => ur.next(sheet.sort(x, asc))
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

  /** Stream of errors in the model */
  val errors = undoRedoSheet
    .filter({_._1.nonEmpty})
    .map({_._1.get})

  /** Stream of sheets, emits whenever an atomic change has happened to the sheet  */
  val sheet = undoRedoSheet
    .map({_._2.current})
}

// TODO: Legacy, refactor tests so they don't need this anymore
class MutableModel() extends Model(BehaviorSubject[ModelMutations](Refresh)) {
  def copyCell(from : CellPos, to : CellPos) {
    sheetMutations.asInstanceOf[BehaviorSubject[ModelMutations]].onNext(CopyCell(from, to))
  }

  def cutCell(from : CellPos, to : CellPos) {
    sheetMutations.asInstanceOf[BehaviorSubject[ModelMutations]].onNext(CutCell(from, to))
  }

  def changeFormula(pos : CellPos, f: String) {
    sheetMutations.asInstanceOf[BehaviorSubject[ModelMutations]].onNext(SetFormula(pos, f))
  }

  def stop(): Unit = {
    sheetMutations.asInstanceOf[BehaviorSubject[ModelMutations]].onCompleted()
  }
}