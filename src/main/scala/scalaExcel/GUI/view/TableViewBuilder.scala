package scalaExcel.GUI.view

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
import scalafx.scene.input.KeyEvent
import scalafx.scene.control.TableColumn.CellEditEvent
import scalafx.geometry.Pos
import scalafx.scene.text.TextAlignment
import scalafx.scene.layout.StackPane

class DataCellColumn(onCellEdit: ((CellPos, String)) => Unit,
                     onColResize: ((Int, Double)) => Unit,
                     colIndex: Int,
                     header: String,
                     headerWidth: Double,
                     onAdd: ((Boolean, Int, Int)) => Unit,
                     onRemove: ((Boolean, Int, Int)) => Unit,
                     onBulkSelect: ((Boolean, Int)) => Unit) extends TableColumn[DataRow, DataCell] {

  text = header
  id = colIndex.toString
  cellValueFactory = _.value.get(colIndex)
  cellFactory = _ => new DataCellView
  prefWidth = headerWidth
  sortable = false

  // insert a special element that wraps the header and handles user clicks
  val columnObject = this
  graphic = new StackPane() {
    // let the element stretch to cover the column header
    prefWidth.bind(columnObject.width.subtract(5))
    val stackObject = this
    // insert into the element a label with the header text
    content = new Label(text.value) {
      style = "-fx-padding: 8px;"
      alignment = Pos.Center
      textAlignment = TextAlignment.Center
      // let the label stretch to cover the element
      prefWidth.bind(stackObject.width)
      // register the bulkOperationsInitiator on the user's click
      onMouseClicked = InteractionHelper.bulkOperationsInitiator(forRows = false,
        this,
        (count:Int, index:Int) => onAdd((false, count, index)),
        (count:Int, index:Int) => onRemove((false, count, index)),
        (index: Int) => {
          // programatically select the whole visible column
          val table = tableView.value
          val selection = table.getSelectionModel
          selection.clearSelection()
          Range(0, table.items.value.size).foreach(r => selection.select(r, columnObject))
          // forward as bulk column selection
          onBulkSelect((false, index))
        })
    }
  }

  // listen for column width changes
  width.onChange {
    (_, _, newWidth) => onColResize((colIndex, newWidth.doubleValue()))
  }

  // listen for cell edits
  onEditCommit = (e: CellEditEvent[DataRow, DataCell]) => {
      val text = e.getNewValue.expression
      // account for numbered column
      val col = e.getTablePosition.getColumn - 1
      val row = e.getTablePosition.getRow
      onCellEdit(((col, row), text))
  }

}

class NumberedColumn(indexConverter: (Int) => Int,
                     colWidth: Int,
                     onAdd: ((Boolean, Int, Int)) => Unit,
                     onRemove: ((Boolean, Int, Int)) => Unit,
                     onBulkSelect: ((Boolean, Int)) => Unit) extends TableColumn[DataRow, DataCell] {
  text = DefaultProperties.NUMBERED_COLUMN_HEADER
  id = "-1"
  cellValueFactory = _ => ObjectProperty(DataCell.newEmpty())
  cellFactory = _ => new TableCell[DataRow, DataCell] {
    // register the bulkOperationsInitiator on the user's click
    onMouseClicked = InteractionHelper.bulkOperationsInitiator(forRows = true,
      this,
      (count:Int, index:Int) => onAdd((true, count, index)),
      (count:Int, index:Int) => onRemove((true, count, index)),
      (index: Int) => {
        // programatically select the whole visible row
        val table = tableView.value
        val selection = table.getSelectionModel
        selection.clearSelection()
        table.columns.foreach (c => selection.select(tableRow.value.getIndex, c))
        // forward as bulk row selection
        onBulkSelect((true, index))
      })
    item.onChange {
      (_, _, _) =>
        // row index must be converted to sheet row index
        text = (indexConverter(tableRow.value.getIndex) + 1).toString
        style = "-fx-alignment: CENTER;"
    }
  }
  minWidth = colWidth
  maxWidth = colWidth
  editable = false
  sortable = false
}

class StreamingTable(labeledTable: LabeledDataTable) {

  type TableColumns = ObservableBuffer[jfxc.TableColumn[DataRow, DataCell]]

  val onCellEdit = Subject[(CellPos, String)]()

  val onColResize = Subject[(Int, Double)]()

  /**
   * Add action subject
   * onNext should be called with tuples of the following nature:
   * (isTheItemBeingAddedARow?, numberOfItemsToBeAdded, smallestIndexOfTheAddedItems)
   */
  val onAdd = Subject[(Boolean, Int, Int)]()

  /**
   * Remove action subject
   * onNext should be called with tuples of the nature
   * (isTheItemBeingRemovedARow?, numberOfItemsToBeRemoved, smallestIndexOfTheRemovedItems)
   */
  val onRemove = Subject[(Boolean, Int, Int)]()

  /**
   * Bulk selection action subject
   * onNext should be called when a user selects an entire row/column
   * with tuples of the following nature:
   * (isTheSelectedCollectionARow?, indexOfTheSelectedCollection)
   */
  val onBulkSelection = Subject[(Boolean, Int)]()

  val table = new TableView[DataRow](labeledTable.data) {
    editable = true
    fixedCellSize = DefaultProperties.FIXED_ROW_HEIGHT

    // the first column is special
    columns += new NumberedColumn(index =>
    //convert table row index to sheet row index
      labeledTable.toSheetIndex((0, index))._2,
      labeledTable.calculateColWidth,
      onAdd.onNext,
      onRemove.onNext,
      onBulkSelection.onNext)

    // add the rest of the columns in the order given by the LabeledDataTable
    columns ++= buildColumns(labeledTable.headers,
      labeledTable.headerWidths)
  }

  val selectionModel = table.getSelectionModel
  selectionModel.setCellSelectionEnabled(true)
  selectionModel.setSelectionMode(SelectionMode.MULTIPLE)

  val onRawSelection = Observable[List[CellPos]](o => {
    selectionModel.getSelectedCells.onChange((source, _) => {
      // first column is -1, because it's reserved for row numbers
      val cells = source
        .toList
        .map { x => (x.getColumn, x.getRow) }
      o.onNext(cells)
    })
  })

  val onSelection = onRawSelection
    .map { _ map (c => (c._1 - 1, c._2)) filter (c => c._1 >= 0 && c._2 >= 0) }
    .map { _ map labeledTable.toSheetIndex }

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
        o.onNext(permutations.map({
          case (c1, c2)  => (labeledTable.toSheetIndex((c1, 0))._1,
            labeledTable.toSheetIndex((c2, 0))._1)
        }))
    })
  })

  val onKeyPressed = Observable[KeyEvent](o => {
    table.onKeyPressed = (e: KeyEvent) => o.onNext(e)
  })

  onKeyPressed.map(_.code)
    .filter(code => !code.isWhitespaceKey &&
      !code.isNavigationKey &&
      !code.isModifierKey &&
      !code.isMediaKey &&
      !code.isFunctionKey)
    .withLatest(onRawSelection.filter(_.size >= 1).map(_.head))
    .subscribe { _ match {
      case ((col, row), key) => table.edit(row, table.columns.get(col))
    }}

  private def buildColumns(headers: List[String],
                            widths: List[Double]): TableColumns = {
    headers.view
      .zip(widths)
      .foldLeft(new TableColumns())((cols, data) => {
      cols += new DataCellColumn(
      {case (pos, formula) =>
          // convert table index to sheet index
          onCellEdit.onNext((labeledTable.toSheetIndex(pos), formula))},
      {case (index, w) =>
          // convert table column index to sheet column index
          onColResize.onNext((labeledTable.toSheetIndex((index, 0))._1, w))},
      cols.length,
      data._1,
      data._2,
      onAdd.onNext,
      onRemove.onNext,
      onBulkSelection.onNext)
    })
  }
}

object TableViewBuilder {

  def build(labeledTable: LabeledDataTable) = {
    new StreamingTable(labeledTable)
  }

}
