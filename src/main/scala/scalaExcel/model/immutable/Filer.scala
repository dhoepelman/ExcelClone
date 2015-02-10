package scalaExcel.model.immutable

import scala.io._
import java.io._
import scalaExcel.model._
import scalafx.scene.paint.Color
import scalaExcel.GUI.util.CSSHelper
import scalaExcel.formula.{VBool, VDouble, VString, Value}
import scala.pickling._
import scala.pickling.Defaults._
import json._


/**
 * Created by Chris on 10-11-2014.
 *
 * The object that handles saving and loading from the file system
 */
object Filer {

  type FileTypeSerialized = (Map[(Int,Int), String], Map[(Int,Int), (String,String,String,String)])
  type FileType = (Map[(Int,Int), String], Map[(Int,Int), Styles])

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

  /** Manually serialize Format case class */
  def serializeFormat(format: ValueFormat): String =
    format match {
      case DefaultValueFormat => "DefaultValueFormat"
      case TextValueFormat => "TextValueFormat"
      case CurrencyValueFormat => "CurrencyValueFormat"
      case ScientificValueFormat => "ScientificValueFormat"
      case PercentageValueFormat => "PercentageValueFormat"
      case CustomNumericValueFormat(prefix, suffix, minInt, maxInt, minFrac, maxFrac, decSymbol, grouping, grSymbol) =>
        "Custom__"+prefix+"__"+suffix+"__"+minInt+"__"+maxInt+"__"+minFrac+"__"+maxFrac+"__"+decSymbol+"__"+grouping+"__"+grSymbol
    }

  /** Manually deserialize Format case class */
  def deserializeFormat(format: String): ValueFormat = {
    format match {
      case "DefaultValueFormat" => DefaultValueFormat
      case "TextValueFormat" => TextValueFormat
      case "CurrencyValueFormat" => CurrencyValueFormat
      case "ScientificValueFormat" => ScientificValueFormat
      case "PercentageValueFormat" => PercentageValueFormat
      case s => try {
        val sections = s.stripPrefix("Custom").split("__")
        CustomNumericValueFormat(sections(0), sections(1), sections(2).toInt, sections(3).toInt, sections(4).toInt, sections(5).toInt, sections(6).headOption, sections(7).toBoolean, sections(8).headOption)
      }
      catch {
        case _: Throwable => new CustomNumericValueFormat()
      }
    }
  }

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
          serializeFormat(s.format))
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
        deserializeFormat(format),
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

  /** Save sheet as CSV file */
  def saveCSV(file: File, sheet: Sheet) = {
    printToFile(file)(_ print sheetToCSV(sheet))
  }

  /** Convert sheet to CSV string */
  def sheetToCSV(sheet: Sheet): String = {
    (0 to sheet.rows).map(row => {
      (0 to sheet.cols).map(column => (sheet.getCellStyle((column, row)), sheet.getValue((column, row))))
        .map({case (styles, value) => escape(styles.format(value))})
        .foldLeft("")((fold, v) => fold + "\"" + v + "\"" + ",")
    }).foldLeft("")((fold, row) => fold + row + "\n")
  }

  /** Covert CSV string to Sheet */
  def CSVToSheet(csv: Array[Array[String]]): Map[(Int, Int), String] = {
    csv.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.zipWithIndex.map { case (value, colIndex) =>
        ((colIndex, rowIndex) -> unescape(value))
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
