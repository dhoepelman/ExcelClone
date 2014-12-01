package scalaExcel.model

import scalafx.scene.paint.Color
import rx.lang.scala.subjects.BehaviorSubject
import scalaExcel.model.OperationHelpers._

class Model {
  /** This is a stream of inputs from 'the world' that will effect the state of the sheet model */
  val sheetMutations = BehaviorSubject[ModelMutations](Refresh)

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

  def updateStyle(sheet: Sheet, x: Int, y: Int, f: Styles => Styles) =
    sheet.setCellStyle(x, y, f(sheet.styles.getOrElse((x, y), Styles.DEFAULT)))

  // this combines the initial Sheet with all input mutations from the outside
  // world
  val sheet = sheetMutations.scan(new Sheet())((sheet, action) => action match {
    case SetFormula(x, y, f) =>
      val (s, updates) = sheet.setCell(x, y, f)
      updateSheet(s, updates, Set((x, y)))
    case EmptyCell(x, y) => updateSheet(sheet.deleteCell((x,y)))
    case CopyCell(from, to) => updateSheet(sheet.copyCell(from, to))
    case CutCell(from, to) => updateSheet(sheet.cutCell(from, to))
    case SetColor(x, y, c) => updateStyle(sheet, x, y, s => s.setColor(c))
    case SetBackground(x, y, c) => updateStyle(sheet, x, y, s => s.setBackground(c))
    case Refresh => sheet
  })

  def refresh() = sheetMutations.onNext(Refresh)

  def emptyCell(x : Int, y : Int)  {
    sheetMutations.onNext(EmptyCell(x,y))
  }

  def copyCell(from : CellPos, to : CellPos) {
    sheetMutations.onNext(CopyCell(from, to))
  }

  def cutCell(from : CellPos, to : CellPos) {
    sheetMutations.onNext(CutCell(from, to))
  }

  def changeFormula(x: Int, y: Int, f: String) {
    sheetMutations.onNext(SetFormula(x, y, f))
  }

  def changeBackground(x: Int, y: Int, c: Color): Unit = {
    sheetMutations.onNext(SetBackground(x, y, c))
  }

  def changeColor(x: Int, y: Int, c: Color): Unit = {
    sheetMutations.onNext(SetColor(x, y, c))
  }
}
