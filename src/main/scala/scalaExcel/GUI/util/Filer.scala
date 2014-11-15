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

  def rowToCSV[T](v : (T => String))(row : Traversable[T]) : String =
    row.foldLeft("")((csv, cell) => csv + v(cell) + ",")

  def gridToCSV[T](v: (T => String))( grid: Traversable[Traversable[T]]) : String =
    grid.foldLeft("")((csv, row) => csv + rowToCSV(v)(row) + "\n")


  def save[T](formatter: (Traversable[Traversable[T]])=>String)
             (file: File, grid: Traversable[Traversable[T]]) = {
    printToFile(file) { _.print(formatter(grid)) }
  }

  private def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
