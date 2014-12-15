package scalaExcel.model.reactive

import java.io.File


import scalaExcel.CellPos
import scalaExcel.model._
import scalaExcel.formula.{VEmpty, Value}
import rx.lang.scala.subjects.BehaviorSubject
import scala.collection.mutable.Map


class Sheet(private val cells : Map[CellPos,BehaviorSubject[CellValue]]) extends scalaExcel.model.Sheet {
  /** Get the number of rows in the sheet */
  override def rows: Int =  if (cells.isEmpty) 0 else cells.keys.maxBy(_._2)._2 + 1

  /** Get the number of columns in the sheet */
  override def cols: Int = if (cells.isEmpty) 0 else cells.keys.maxBy(_._1)._1 + 1

  /** Get a tuple of columns and rows in the sheet */
  override def size: (Int, Int) = (rows, cols)

  def getCellSubject(pos : CellPos) = {
    if(!cells.contains(pos)) {

    }
  }

  val emptyCell = CellValue(Cell(), VEmpty, Styles.DEFAULT)
  def createCell(pos : CellPos) = cells.put(pos, new BehaviorSubject[CellValue](emptyCell))

  /** Value of a Cell */
  override def getValue(pos: (Int, Int)): Value = ???

  /** Styles of a Cell */
  override def getCellStyle(pos: (Int, Int)): Styles = ???

  /** Get the Cell if it exists or return an empty cell */
  override def getCell(pos: (Int, Int)): Cell = ???



  /** Save this sheet to a file */
  override def saveTo(file: File): Unit = ???
}

case class CellValue(cell : Cell, value : Value, style : Styles)