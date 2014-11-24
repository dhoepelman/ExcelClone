package scalaExcel.GUI.util

import java.io.File

import scala.io._
import scalaExcel.GUI.data.{LabeledDataTable, DataCell}
import LabeledDataTable.DataTable
import scalaExcel.GUI.data.DataCell

/**
 * Created by Chris on 10-11-2014.
 *
 * The object that handles saving and loading from the file system
 */
object Filer {

  def cellToCSV(cell: DataCell) : String =  cell.expression
  def stringToCSV(cell: String) : String = cell

  def rowToCSV[T](v : (T => String))(row : Traversable[T]) : String =
    row.foldLeft("")((csv, cell) => csv + v(cell) + ",")

  def gridToCSV[T](v: (T => String))( grid: Traversable[Traversable[T]]) : String =
    grid.foldLeft("")((csv, row) => csv + rowToCSV(v)(row) + "\n")


  def save[T](formatter: (Traversable[Traversable[T]])=>String)
             (file: File, grid: Traversable[Traversable[T]]) = {
    printToFile(file) { _.print(formatter(grid)) }
  }


  def toStringTable(dataTable: DataTable) : Traversable[Traversable[String]] =
    dataTable.map(_.map(_.value.expression))

  def saveCSV(file: java.io.File, dataTable: DataTable) = {
    val data = toStringTable(dataTable)
    Filer.save[String](Filer.gridToCSV(identity))(file, data)
  }


  def loadCSV(file: java.io.File) : List[List[String]] =
    Source.fromFile(file).getLines()
          .map(line => line.split(",").toList)
          .toList


  private def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
