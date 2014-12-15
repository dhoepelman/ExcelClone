package scalaExcel.model

import java.io.File

import scalaExcel.formula.Value

trait Sheet {

  /** Get the number of rows in the sheet */
  def rows: Int
  /** Get the number of columns in the sheet */
  def cols: Int
  /** Get a tuple of columns and rows in the sheet */
  def size: (Int, Int)

  /** Get the Cell if it exists or return an empty cell */
  def getCell(pos: (Int, Int)): Cell

  /** Value of a Cell */
  def getValue(pos: (Int, Int)): Value

  /** Styles of a Cell */
  def getCellStyle(pos: (Int, Int)): Styles

  /** Save this sheet to a file */
  def saveTo(file: File): Unit
}
