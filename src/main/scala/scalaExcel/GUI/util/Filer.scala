package scalaExcel.GUI.util

import java.io.File

import scalaExcel.GUI.model.SheetCell

import scala.io._

/**
 * Created by Chris on 10-11-2014.
 *
 * The object that handles saving and loading from the file system
 */
object Filer {

  def cellToCSV(cell: SheetCell) : String =  cell.expression
  def stringToCSV(cell: String) : String = cell

  /**
   *
   * @param v A function that produces the string that represents the cell
   * @param row A row of cells
   * @tparam T The type of the cell
   * @return The CSV row
   */
  def rowToCSV[T](v : (T => String))(row : Iterable[T]) : String =
    row.foldLeft("")((csv, cell) => csv + v(cell) + ",")

  /**
   *
   * @param v A function that produces the string that represents a cell
   * @param grid A grid of cells
   * @tparam T The type of the cell
   * @return The CSV files
   */
  def gridToCSV[T](v: (T => String))( grid: Iterable[Iterable[T]]) : String =
    grid.foldLeft("")((csv, row) => csv + rowToCSV(v)(row) + "\n")


  def save[T](formatter: (Iterable[Iterable[T]])=>String)
             (file: File, grid: Iterable[Iterable[T]]) = {
    printToFile(file) { _.print(formatter(grid)) }
  }

  private def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
