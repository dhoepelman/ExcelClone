package scalaExcel.model

import scala.io._
import java.io._
import scalafx.scene.paint.Color
import scalaExcel.GUI.util.CSSHelper
import scalaExcel.formula.{VBool, VDouble, VString, Value}
import scala.pickling._
import scala.pickling.json._
import json._


/**
 * Created by Chris on 10-11-2014.
 *
 * The object that handles saving and loading from the file system
 */
object Filer {

  type FileTypeSerialized = (Map[(Int,Int), String], Map[(Int,Int), (String,String,String,String)])
  type FileType = (Map[(Int,Int), String], Map[(Int,Int), Styles])

  /** Make Filer load functionality accessible directly on the model */
  implicit class ModelExtension(model: Model) {
    def loadFrom(file: File): Unit = {
      val data = Filer.load(file)
      model.clearAndSet(data._1, data._2)
    }
  }

  /** Make Filer save functionality accessible directly on the sheet */
  implicit class SheetExtension(sheet: Sheet) {
    def saveTo(file: File): Unit = {
      Filer.save(file, sheet)
    }
  }


  /** Saves the sheet to the file.
    * File format is inferred by the file extension */
  def save(file: File, sheet: Sheet): Unit = {
    fileExtension(file.getName).toLowerCase match {
      case "csv" => saveCSV(file, sheet)
      case "scalaexcel" => saveHomebrew(file, sheet)
    }
  }

  /** Load from file
    * File format is inferred by the file extension */
  def load(file: File) : FileType = {
    fileExtension(file.getName).toLowerCase match {
      case "csv" => loadCSV(file)
      case "scalaexcel" => loadHomebrew(file)
    }
  }

  /** The the extension part of a name, not including the dot */
  def fileExtension(filename: String): String = {
    filename.substring(filename.lastIndexOf('.') + 1)
  }



  /** Manually serialize Alignment case class */
  def serializeAlignment(alignment: Alignment): String = {
    alignment match {
      case LeftAlign   => "LeftAlign"
      case CenterAlign => "CenterAlign"
      case RightAlign  => "RightAlign"
      case NoAlign     => "NoAlign"
    }
  }

  /** Manually deserialize Alignment case class */
  def deserializeAlignment(alignment: String): Alignment = {
    alignment match {
      case "LeftAlign"   => LeftAlign
      case "CenterAlign" => CenterAlign
      case "RightAlign"  => RightAlign
      case "NoAlign"     => NoAlign
    }
  }

  /** Manually serialize Color */
  def serializeColour(colour: Color): String = CSSHelper.colorToWeb(colour)

  /** Manually deserialize Color */
  def deserializeColour(colour: String): Color = Color.web(colour)

  /** Save sheet to custom file type that preserves all the features */
  def saveHomebrew(file: File, sheet: Sheet): Unit = {
    // Pickling Sheet itself causes compiler to hang
    // Pickling Color class produces empty pickle
    // Pickling case class not working
    try {
      val structure : FileTypeSerialized = (
        sheet.cells.mapValues(_.f),
        sheet.styles.mapValues(s => (
          serializeAlignment(s.align),
          serializeColour(s.background),
          serializeColour(s.color),
          s.format)
        )
      )
      printToFile(file)(_.print(structure.pickle.value))
      println("Save completed")
    } catch {
      case e: Throwable => println("Moooom, scala.pickling is broken again: " + e)
    }
  }

  /** Load from a file of the custom file type */
  def loadHomebrew(file: File) : FileType = {
    val source = Source.fromFile(file)
    val json: String = source.mkString
    source.close()
    val data = JSONPickle(json).unpickle[FileTypeSerialized]
    (data._1, data._2.mapValues{
      case (align, background, colour, format) => new Styles(
        deserializeColour(background),
        deserializeColour(colour),
        format,
        deserializeAlignment(align)
      )
    })
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
  def saveCSV(file: File, sheet: Sheet) = {
    printToFile(file)(_ print sheetToCSV(sheet))
  }

  /** Convert sheet to CSV string */
  def sheetToCSV(sheet: Sheet): String = {
    (0 to sheet.rows).map(row => {
      (0 to sheet.cols).map(column => sheet.valueAt((column, row))).map({
        case Some(v) => escape(dummyFormat(v)) //TODO hook to proper formatter
        case _ => ""
      }).foldLeft("")((fold, v) => fold + "\"" + v + "\"" + ",")
    }).foldLeft("")((fold, row) => fold + row + "\n")
  }

  /** Covert CSV string to Sheet */
  def CSVToSheet(csv: Array[Array[String]]): Map[(Int, Int), String] = {
    csv.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.zipWithIndex.map { case (value, colIndex) =>
        ((rowIndex, colIndex) -> unescape(value))
      }
    }.toMap
  }

  /** Get the contents of a csv file */
  def loadCSV(file: File): FileType = {
    val linesOfTokens = Source.fromFile(file).getLines()
      // http://stackoverflow.com/a/769713/661190
      .map(line => line.split( """,(?=(?:[^\"]*\"[^\"]*\")*(?![^\"]*\"))""").map(_.trim.stripPrefix("\"").stripSuffix("\"")).toArray)
      .toArray
    (CSVToSheet(linesOfTokens), Map())
  }



  /** Generic printer to file */
  private def printToFile(f: File)(op: java.io.PrintWriter => Unit) = {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }

}
