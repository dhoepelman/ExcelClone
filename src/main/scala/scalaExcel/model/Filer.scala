package scalaExcel.model

import scala.io._
import scalaExcel.formula.{VBool, VDouble, VString, Value}


/**
 * Created by Chris on 10-11-2014.
 *
 * The object that handles saving and loading from the file system
 */
object Filer {

  /**
   * Make Filer functionality accessible directly on the model
   * @param model
   */
  implicit class ModelExtension(model: Model) {
    def loadFrom(file: java.io.File): Unit = {
      model.clearAndSet(Filer.loadCSV(file))
    }
  }

  implicit class SheetExtension(sheet: Sheet) {
    def saveTo(file: java.io.File): Unit = {
      Filer.saveCSV(file, sheet)
    }
  }

  /** Escape for csv */
  def escape(s: String): String =
    s.replace("\"", "\"\"")

  /** Unescape form csv */
  def unescape(s: String): String =
    s.replace("\"\"", "\"")

  /** A very basic formatter */
  def dummyFormat(value: Value): String = {
    //TODO remove function when formatter is available
    value match {
      case VDouble(v) => v.toString
      case VString(v) => v.toString
      case VBool(v) => v.toString
      case _ => ""
    }
  }

  /** Save sheet as CSV file */
  def saveCSV(file: java.io.File, sheet: Sheet) = {
    printToFile(file)(_ print sheetToCSV(sheet))
  }

  /** Convert sheet to CSV string */
  def sheetToCSV(sheet: Sheet): String = {
    (0 to sheet.rows).map(row => {
      (0 to sheet.cols).map(column => sheet.getValue((column, row))).map(v =>
         escape(dummyFormat(v)) //TODO hook to proper formatter
      ).foldLeft("")((fold, v) => fold + "\"" + v + "\"" + ",")
    }).foldLeft("")((fold, row) => fold + row + "\n")
  }

  /** Covert CSV string to Sheet */
  def CSVToSheet(csv: Array[Array[String]]): Traversable[((Int, Int), String)] = {
    csv.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.zipWithIndex.map { case (value, colIndex) =>
        ((colIndex, rowIndex), unescape(value))
      }
    }
  }

  /** Get the contents of a csv file */
  def loadCSV(file: java.io.File): Traversable[((Int, Int), String)] = {
    val linesOfTokens = Source.fromFile(file).getLines()
      // http://stackoverflow.com/a/769713/661190
      .map(line => line.split( """,(?=(?:[^\"]*\"[^\"]*\")*(?![^\"]*\"))""").map(_.trim.stripPrefix("\"").stripSuffix("\"")).toArray)
      .toArray
    CSVToSheet(linesOfTokens)
  }

  /** Generic printer to file */
  private def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }

}
