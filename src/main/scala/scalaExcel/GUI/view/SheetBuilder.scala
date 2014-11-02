package scalaExcel.GUI.view

import scalafx.scene.control._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.cell.TextFieldTableCell
import javafx.scene.{control => jfxsc}
import scalaExcel.GUI.model.SheetCell
import scalaExcel.GUI.model.DataModelFactory.{DataTable, DataRow}
import scalaExcel.GUI.model.SheetCellStringConverter.SheetCellStringConverter

object SheetBuilder {
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
            val inner = new jfxsc.cell.TextFieldTableCell[DataRow, SheetCell](new SheetCellStringConverter) {
              override def startEdit(): Unit = {
                super.startEdit()
                //TODO coordinate formulaEditor with this field
                val textField = new TextField(this.getChildren.get(0).asInstanceOf[jfxsc.TextField])
                textField.text = getItem.expr
              }
            }
            new TextFieldTableCell[DataRow, SheetCell](inner) {
              item.onChange {
                (_, _, newCell) =>
                  style = if (newCell == null) "" else newCell.stylist(null)
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
