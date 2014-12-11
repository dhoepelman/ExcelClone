package scalaExcel.model

import rx.lang.scala.Observable
import rx.lang.scala.subjects.BehaviorSubject
import scalaExcel.model.Sorter.SheetSorter
import scalaExcel.CellPos

/**
 * Represents the data model of the ScalaExcel application
 * @param sheetMutations Inputs from outside this package that will effect the state of the sheet model
 */
class Model(protected val sheetMutations : Observable[ModelMutations]) {
  /** Perform a modification on the sheet */
  private def modifySheet(ur: UndoRedo[Sheet], action: ModelMutations) = {
    val sheet = ur.current
    action match {
      case SetFormula(pos, f) =>
        ur.next(sheet.setCell(pos, f))
      case EmptyCell(poss) =>
        ur.next(sheet.deleteCells(poss))
      case CopyCell(from, to) =>
        ur.next(sheet.copyCell(from, to))
      case CutCell(from, to) =>
        ur.next(sheet.cutCell(from, to))
      case SetColor(poss, c) =>
        ur.next(sheet.updateCellsStyle(poss, s => s.setColor(c)))
      case SetBackground(poss, c) =>
        ur.next(sheet.updateCellsStyle(poss, s => s.setBackground(c)))
      case SetSheet(values, styles) =>
        ur.next(values.foldLeft(new Sheet(Map(), Map(), Map(), styles)) {
          case (sheet, (pos, value)) => sheet.setCell(pos, value)
        })
      case SortColumn(x, asc) =>
        ur.next(sheet.sort(x, asc))
      case Undo =>
        ur.undo()
      case Redo =>
        ur.redo()
      case Refresh =>
        ur
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