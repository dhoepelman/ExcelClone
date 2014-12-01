package scalaExcel.GUI.view

import javafx.event.EventHandler
import javafx.scene.input.{MouseButton, MouseEvent}
import javafx.scene.{control => jfxc}

import rx.lang.scala.{Observable, Subject}

import scalaExcel.CellPos
import scalaExcel.GUI.data.LabeledDataTable.DataRow
import scalaExcel.GUI.data.{DataCell, LabeledDataTable}
import scalaExcel.util.DefaultProperties
import scalaExcel.rx.operators.WithLatest._

import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._

class DataCellColumn(onCellEdit: ((CellPos, String)) => Unit,
                     onColResize: ((Int, Double)) => Unit,
                     colIndex: Int,
                     header: String,
                     headerWidth: Double) extends TableColumn[DataRow, DataCell] {

  text = header
  id = colIndex.toString
  cellValueFactory = _.value.get(colIndex)
  cellFactory = _ => new DataCellView
  prefWidth = headerWidth
  sortable = false

  // listen for column width changes
  width.onChange {
    (_, _, newWidth) => onColResize((colIndex, newWidth.doubleValue()))
  }

  // listen for cell edits
  onEditCommit = new EventHandler[jfxc.TableColumn.CellEditEvent[DataRow, DataCell]] {
    override def handle(e: jfxc.TableColumn.CellEditEvent[DataRow, DataCell]) = {
      val text = e.getNewValue.expression
      // account for numbered column
      val col = e.getTablePosition.getColumn - 1
      val row = e.getTablePosition.getRow
      onCellEdit(((col, row), text))
    }
  }

}

class NumberedColumn extends TableColumn[DataRow, DataCell] {
  text = DefaultProperties.NUMBERED_COLUMN_HEADER
  id = "-1"
  cellValueFactory = _ => ObjectProperty(DataCell.newEmpty())
  cellFactory = _ => new TableCell[DataRow, DataCell] {
    item.onChange {
      (_, _, _) =>
        text = (tableRow.value.getIndex + 1).toString
        style = "-fx-alignment: CENTER;"
    }
  }
  prefWidth = DefaultProperties.NUMBERED_COLUMN_WIDTH
  editable = false
  sortable = false
}

class StreamingTable(labeledTable: LabeledDataTable) {

  type TableColumns = ObservableBuffer[jfxc.TableColumn[DataRow, DataCell]]

  val table = new TableView[DataRow](labeledTable.data) {
    editable = true

    // the first column is special
    columns += new NumberedColumn
    // add the rest of the columns in the order given by the LabeledDataTable
    columns ++= buildColumns(labeledTable.headers,
      labeledTable.headerWidths)
  }

  val selectionModel = table.getSelectionModel
  selectionModel.setCellSelectionEnabled(true)
  selectionModel.setSelectionMode(SelectionMode.MULTIPLE)

  val onSelection = Observable[List[CellPos]](o => {
    selectionModel.getSelectedCells.onChange((source, _) => {
      // first column is -1, because it's reserved for row numbers
      val cells = source
        .toList
        .map { x => (x.getColumn - 1, x.getRow) }
        .filter {
          case (col, row) => col >= 0 && row >= 0
        }
      o.onNext(cells)
    })
  })

  val onColumnReorder = Observable[Map[Int, Int]](o => {
    table.columns.onChange((cols, changes) => {
      val permutations = cols
        .view
        .zipWithIndex
        .foldLeft(Map[Int, Int]())((acc, indexedCol) => {
        // compare id to index in cols and account for numbered column
        if (indexedCol._1.getId.toInt == indexedCol._2 - 1) acc
        else acc + (indexedCol._1.getId.toInt -> (indexedCol._2 - 1))
      })
      // notify manager of change
      if (!permutations.keySet.contains(-1))
        o.onNext(permutations)
    })
  })

  val onCellEdit = Subject[(CellPos, String)]()

  val onColResize = Subject[(Int, Double)]()

  val onClick = Observable[MouseEvent](o => {
    table.onMouseClicked = new EventHandler[MouseEvent] {
      override def handle(event: MouseEvent) {
        o.onNext(event)
      }
    }
  })

  val onRightClick = onClick
    .filter(event => {
    (event.getButton.compareTo(MouseButton.SECONDARY) == 0)
  })

  private def buildColumns(
                            headers: List[String],
                            widths: List[Double]): TableColumns = {

    headers.view
      .zip(widths)
      .foldLeft(new TableColumns())((cols, data) => {
      cols += new DataCellColumn(
        (onCellEdit.onNext(_)),
        (onColResize.onNext(_)),
        cols.length,
        data._1,
        data._2)
    })
  }

  /** Observable of selected cells in the table */
  val selectedCellStream = Observable[List[CellPos]]({ o =>
    table.selectionModel.value.getSelectedCells.onChange({ (poss, _) => o.onNext(poss.toList map ({
      pos => (pos.getColumn - 1, pos.getRow)
    }))
    })
  })

  /** Combine an observable with the current selected cells in the table */
  def withSelectedCells[T](o: Observable[T]): Observable[(List[CellPos], T)] = o.withLatest(selectedCellStream)

  // Grumble grumble JVM type erasure can't overload on Observable[Unit] and Observable[T] grumble grumble
  def withSelectedCellsOnly(o: Observable[Any]): Observable[List[CellPos]] = withSelectedCells(o).map({ case (ps,_) => ps })

}

object TableViewBuilder {

  def build(labeledTable: LabeledDataTable) = {
    new StreamingTable(labeledTable)
  }

}
