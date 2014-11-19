package scalaExcel.GUI.view

import scalafx.scene.control._
import scalafx.collections.ObservableBuffer
import scalaExcel.GUI.modelwrapper.SheetCell
import javafx.scene.{control => jfxc}
import scalaExcel.GUI.controller.{LabeledDataTable, Mediator}
import scalaExcel.GUI.controller.LabeledDataTable.DataRow

class SheetCellColumn(colIndex: Int, header: String, headerwidth: Double) extends TableColumn[DataRow, SheetCell] {
  text = header
  id = colIndex.toString
  cellValueFactory = _.value.get(colIndex)
  cellFactory = {
    column => new SheetCellView()
  }
  prefWidth = headerwidth
}

object TableViewBuilder {
  type TableColumns = ObservableBuffer[jfxc.TableColumn[DataRow, SheetCell]]

  private def buildColumns(headers: List[String], widths: List[Double]): TableColumns =
    headers.view.zip(widths).foldLeft(new TableColumns())((cols: TableColumns, data: (String, Double)) =>
      cols += new SheetCellColumn(cols.length, data._1, data._2))

  def build(labeledTable: LabeledDataTable) = {
    new TableView[DataRow](labeledTable.data) {
      editable = true
      columns ++= buildColumns(labeledTable.headers, labeledTable.headerWidths)
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