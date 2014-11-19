package scalaExcel.GUI.view

import scalafx.scene.control._
import scalafx.collections.ObservableBuffer
import scalaExcel.GUI.modelwrapper.SheetCell
import scalaExcel.GUI.modelwrapper.DataModelFactory.{DataTable, DataRow}
import javafx.scene.{control => jfxc}
import scalaExcel.GUI.controller.Mediator

class SheetCellColumn(colIndex: Int, colHeader: String, colWidth: Double) extends TableColumn[DataRow, SheetCell] {
  text = colHeader
  id = colIndex.toString
  cellValueFactory = _.value.get(colIndex)
  cellFactory = {
    column => new SheetCellView()
  }
  prefWidth = colWidth
}

object TableViewBuilder {
  type TableColumns = ObservableBuffer[jfxc.TableColumn[DataRow, SheetCell]]

  private val defaultHeaders = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  private val defaultWidths = List(100, 200, 100, 100, 100, 100, 100, 100, 100, 100)

  private def buildColumns(headers: List[String], widths: List[Int]): TableColumns =
    headers.view.zip(widths).foldLeft(new TableColumns())((cols: TableColumns, data: (String, Int)) =>
      cols += new SheetCellColumn(cols.length, data._1, data._2))

  def build(columnHeaders: List[String], columnWidths: List[Int], rows: DataTable) = {
    new TableView[DataRow](rows) {
      editable = true
      val headers = if (columnHeaders == null) defaultHeaders else columnHeaders
      val widths = if (columnWidths == null) defaultWidths else columnWidths
      columns ++= buildColumns(headers, widths)
      columns.onChange((cols, changes) => {
        val (permutations, newCols) = cols.view.zipWithIndex.foldLeft((Map[Int, Int](), new TableColumns()))((acc, indexedCol) =>
          if (indexedCol._1.getId == indexedCol._2.toString)
            (acc._1, acc._2 :+ indexedCol._1.asInstanceOf[jfxc.TableColumn[DataRow, SheetCell]])
          else
            (acc._1 + (indexedCol._1.getId.toInt -> indexedCol._2), acc._2 :+ new SheetCellColumn(indexedCol._2.toInt, indexedCol._1.getText, indexedCol._1.getPrefWidth).delegate))
        if (permutations.size > 0) {
          Mediator.columnsReordered(permutations)
          //TODO activate if model changes on reorder
          //          columns.setAll(newCols.zip(cols).map(pair => if (pair._1 == null) pair._2 else pair._1))
        }
      })
    }
  }
}