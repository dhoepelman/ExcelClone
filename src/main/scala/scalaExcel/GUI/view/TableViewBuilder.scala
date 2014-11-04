package scalaExcel.GUI.view

import scalafx.scene.control._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.cell.TextFieldTableCell
import javafx.scene.{control => jfxsc}
import scalaExcel.GUI.model.SheetCell
import scalaExcel.GUI.model.DataModelFactory.{DataTable, DataRow}
import SheetCellStringConverter.SheetCellStringConverter
import scalaExcel.GUI.controller.Mediator
import scalaExcel.GUI.util.{CircularEvaluation, ErroneousEvaluation}

object TableViewBuilder {
  type TableColumns = ObservableBuffer[javafx.scene.control.TableColumn[DataRow, SheetCell]]

  private val defaultHeaders = List("A", "B")
  private val defaultWidths = List(100, 200)

  def getColumnList(headers: List[String], widths: List[Int]): TableColumns =
    getColumnListReversed(headers.reverse, widths.reverse)

  def getColumnListReversed(headers: List[String], widths: List[Int]): TableColumns = {
    if (headers == List())
      new TableColumns()
    else
      getColumnListReversed(headers.tail, widths.tail) += new TableColumn[DataRow, SheetCell] {
        text = headers.head
        cellValueFactory = {
          _.value.get(headers.length - 1)
        }
        cellFactory = {
          column =>
            new TextFieldTableCell[DataRow, SheetCell](new SheetCellView) {
              item.onChange {
                (_, _, newCell) =>
                  // apply cell customization
                  style = {
                    if (newCell == null)
                      ""
                    else newCell.evaluated match {
                      case x: ErroneousEvaluation => SheetCell.makeError(null)
                      case x: CircularEvaluation => SheetCell.makeError(null)
                      case _ => newCell.stylist(null)
                    }
                  }
              }
            }
        }
        prefWidth = widths.head
      }
  }

  def build(columnHeaders: List[String], columnWidths: List[Int], rows: DataTable) = {
    new TableView[DataRow](rows) {
      editable = true
      val headers = if (columnHeaders == null) defaultHeaders else columnHeaders
      val widths = if (columnWidths == null) defaultWidths else columnWidths
      columns ++= getColumnList(headers, widths)
    }
  }
}
