package scalaExcel.GUI.view

import scalafx.scene.control._
import scalafx.collections.ObservableBuffer
import scalaExcel.GUI.modelwrapper.SheetCell
import javafx.scene.{control => jfxc}
import scalaExcel.GUI.controller.{LabeledDataTable, Mediator}
import scalaExcel.GUI.controller.LabeledDataTable.DataRow
import scalafx.beans.property.ObjectProperty

class SheetCellColumn(colIndex: Int, header: String, headerWidth: Double) extends TableColumn[DataRow, SheetCell] {
  text = header
  id = colIndex.toString
  cellValueFactory = _.value.get(colIndex)
  cellFactory = _ => new SheetCellView(this)
  prefWidth = headerWidth
}

class NumberedColumn extends TableColumn[DataRow, SheetCell] {
  text = "#"
  id = "-1"
  cellValueFactory = _ => ObjectProperty.apply(SheetCell.newEmpty())
  cellFactory = _ => new TableCell[DataRow, SheetCell] {
    item.onChange {
      (_, _, _) =>
        text = (tableRow.value.getIndex + 1).toString
        style = "-fx-alignment: CENTER;"
    }
  }
  prefWidth = 35
  editable = false
}

object TableViewBuilder {
  type TableColumns = ObservableBuffer[jfxc.TableColumn[DataRow, SheetCell]]

  private def buildColumns(headers: List[String], widths: List[Double]): TableColumns =
    headers.view.zip(widths).foldLeft(new TableColumns())((cols: TableColumns, data: (String, Double)) =>
      cols += new SheetCellColumn(cols.length, data._1, data._2))

  def build(labeledTable: LabeledDataTable) = {
    new TableView[DataRow](labeledTable.data) {
      editable = true
      columns += new NumberedColumn
      columns ++= buildColumns(labeledTable.headers, labeledTable.headerWidths)
      columns.onChange((cols, changes) => {
        val permutations = cols.view.zipWithIndex.foldLeft(Map[Int, Int]())((acc, indexedCol) =>
          if (indexedCol._1.getId == indexedCol._2.toString) acc
          else acc + (indexedCol._1.getId.toInt -> indexedCol._2))
        if (!permutations.keySet.contains(0))
          Mediator.rebuildTable()
        else if (permutations.size > 0) {
          Mediator.columnsReordered(permutations)
        }
      })
    }
  }
}