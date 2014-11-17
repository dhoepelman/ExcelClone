package scalaExcel.GUI.view

import scalafx.scene.control._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TableColumn._
import scalaExcel.GUI.model.SheetCell
import scalaExcel.GUI.model.DataModelFactory.{DataTable, DataRow}

object TableViewBuilder {
  type TableColumns = ObservableBuffer[javafx.scene.control.TableColumn[DataRow, SheetCell]]

  private val defaultHeaders = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  private val defaultWidths = List(100, 200, 100, 100, 100, 100, 100, 100, 100, 100)

  def buildColumns(headers: List[String], widths: List[Int]): TableColumns = {
    headers.view.zip(widths).foldLeft(new TableColumns())((cols: TableColumns, data: (String, Int)) =>
      cols +=
        new TableColumn[DataRow, SheetCell] {
          val columnIndex = cols.length
          text = data._1
          cellValueFactory = _.value.get(columnIndex)
          cellFactory = {
            column => new SheetCellView()
          }
          prefWidth = data._2
        })

  }

  def build(columnHeaders: List[String], columnWidths: List[Int], rows: DataTable) = {
    new TableView[DataRow](rows) {
      editable = true
      val headers = if (columnHeaders == null) defaultHeaders else columnHeaders
      val widths = if (columnWidths == null) defaultWidths else columnWidths
      columns ++= buildColumns(headers, widths)
    }
  }
}
