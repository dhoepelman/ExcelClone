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
  /** This is a stream of inputs from 'the world' that will effect the state of the sheet model */
  val sheetMutations = BehaviorSubject[ModelMutations](Refresh)

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