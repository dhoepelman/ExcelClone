package scalaExcel.GUI.view

import scalafx.scene.control._
import scalafx.collections.ObservableBuffer
import scalaExcel.GUI.modelwrapper.SheetCell
import javafx.scene.{control => jfxc}
import scalaExcel.GUI.controller.{LabeledDataTable, Mediator}
import scalaExcel.GUI.controller.LabeledDataTable.DataRow
import scalafx.beans.property.ObjectProperty
import javafx.event.EventHandler
import scalafx.scene.control.TableColumn.SortType
import javafx.scene.input.{MouseButton, MouseEvent}

class SheetCellColumn(colIndex: Int, header: String, headerWidth: Double, sorted: Boolean, ascending: Boolean) extends TableColumn[DataRow, SheetCell] {
  text = header
  id = colIndex.toString
  cellValueFactory = _.value.get(colIndex)
  cellFactory = _ => new SheetCellView(this)
  prefWidth = headerWidth
  if (sorted)
    if (ascending)
      sortType = SortType.ASCENDING
    else
      sortType = SortType.DESCENDING
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
  sortable = false
}

object TableViewBuilder {
  type TableColumns = ObservableBuffer[jfxc.TableColumn[DataRow, SheetCell]]

  private def buildColumns(headers: List[String], widths: List[Double], sortColumn: Int, sortAscending: Boolean): TableColumns =
    headers.view.zip(widths).foldLeft(new TableColumns())((cols: TableColumns, data: (String, Double)) =>
      cols += new SheetCellColumn(cols.length, data._1, data._2, cols.length == sortColumn, sortAscending))

  def build(labeledTable: LabeledDataTable) = {
    new TableView[DataRow](labeledTable.data) {
      editable = true

      //
      // Add columns
      //

      // the first column is special
      columns += new NumberedColumn
      // add the rest of the columns in the order given by the LabeledDataTable
      columns ++= buildColumns(labeledTable.headers, labeledTable.headerWidths, labeledTable.sortColumn, labeledTable.sortAscending)
      // when the order of the columns changes, notify Mediator of new order
      columns.onChange((cols, changes) => {
        val permutations = cols.view.zipWithIndex.foldLeft(Map[Int, Int]())((acc, indexedCol) => {
          //compare id to index in cols and account for numbered column
          println("On position " + indexedCol._2 + " found " + indexedCol._1.getId)
          if (indexedCol._1.getId.toInt == indexedCol._2 - 1) acc
          else acc + (indexedCol._1.getId.toInt -> (indexedCol._2 - 1))
        })
        println("Permutations: " + permutations)
        if (!permutations.keySet.contains(-1))
        // notify Mediator of change
          Mediator.columnsReordered(permutations)
        else
        // revert reordering (numbered column was moved)
          Mediator.rebuildTable()
      })

      //
      // Handle sorting
      //

      // first only disable sorting
      onSort = new EventHandler[jfxc.SortEvent[jfxc.TableView[DataRow]]] {
        override def handle(event: jfxc.SortEvent[jfxc.TableView[DataRow]]) {
          event.consume()
        }
      }
      // set the sort column in the table's sort order (to make sort arrow visible)
      if (labeledTable.sortColumn >= 0)
        sortOrder.add(columns.drop(labeledTable.sortColumn + 1).head)
      // finally, handle sort events, but do not let them propagate to GUI
      onSort = new EventHandler[jfxc.SortEvent[jfxc.TableView[DataRow]]] {
        override def handle(event: jfxc.SortEvent[jfxc.TableView[DataRow]]) {
          val columns = event.getSource.getSortOrder
          if (columns.size() > 0) {
            // sorting should be applied
            val column = columns.get(0)
            Mediator.rowsSorted(column.getId.toInt, column.getSortType == jfxc.TableColumn.SortType.ASCENDING)
          }
          else {
            // sorting should not be applied anymore
            Mediator.rowsSorted(-1, sortAscending = true)
          }
          event.consume()
        }
      }
      onMouseClicked = new EventHandler[MouseEvent] {
        override def handle(event: MouseEvent) {
          if (event.getButton.compareTo(MouseButton.SECONDARY) == 0) {
            println("right clicked!")
            //TODO add contextual menu for new row / new column
            println(event.getSource.asInstanceOf[jfxc.TableView[DataRow]].getSelectionModel.getSelectedIndex)
          }
        }
      }
    }
  }
}