package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalaExcel.formula.{VEmpty, numToCol}
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.ObjectProperty

import scalaExcel.model.{Sheet, Styles}
import scalaExcel.util.DefaultProperties
import scalaExcel.GUI.data.DataWindow.{Bounds, Offsets}

/**
 * Wrapper on the data model sheet
 *
 * It contains all sheet data, but also information on the current window,
 * the column headers and widths, the current sort configuration, etc.
 *
 * The 'rebuild' property shows if the table has changed its structure
 * and needs to be rebuilt, or only its contents need updating
 */
class LabeledDataTable(
                        _dataWindow: DataWindow = LabeledDataTable.defaultDataWindow,
                        _allHeaderWidths: List[Double] = LabeledDataTable.defaultHeaderWidths,
                        _sheet: Sheet = null,
                        _sortColumn: Int = -1,
                        val sortAscending: Boolean = true,
                        val rebuild: Boolean) {

  def headers = LabeledDataTable.getHeaders(_dataWindow.visibleBounds)

  def headerWidths = _allHeaderWidths.drop(_dataWindow.visibleBounds._1).take(_dataWindow.columnCount)

  def sortColumn = _dataWindow.absoluteToWindowColumn(_sortColumn)

  def toSheetIndex = _dataWindow.windowToAbsolute _

  def toTableIndex = _dataWindow.absoluteToWindow _

  def slideWindowBy(offsets: Offsets) =
    new LabeledDataTable(_dataWindow.slideBy(offsets),
      _allHeaderWidths,
      _sheet,
      _sortColumn,
      sortAscending,
      rebuild = true)

  def slideWindowTo(bounds: Bounds) =
    new LabeledDataTable(_dataWindow.slideTo(bounds),
      _allHeaderWidths,
      _sheet,
      _sortColumn,
      sortAscending,
      rebuild = true)

  def dataCellFromSheet(index: CellPos) = {
    // function that builds a DataCell based on a window index
    if (_sheet == null)
    // there is no data yet, only empty cells
      DataCell.newEmpty()
    else
    // build a DataCell with the contents of the sheet at that position
      DataCell.newEvaluated(
        _sheet.cells.get(index) match {
          case Some(c) => c.f
          case None => ""
        },
        _sheet.valueAt(index).getOrElse(VEmpty),
        _sheet.styles.getOrElse(index, Styles.DEFAULT))
  }

  def dataCellFromWindow(index: CellPos) = dataCellFromSheet(toSheetIndex(index))

  val data =
    LabeledDataTable.buildDataTable(
      _dataWindow.rowCount,
      _dataWindow.columnCount,
      dataCellFromWindow)

  def updateContents(sheet: Sheet) = {
    new LabeledDataTable(_dataWindow.expandTo(sheet.size),
      _allHeaderWidths,
      sheet,
      _sortColumn,
      sortAscending,
      rebuild = false)
  }

  def updateWindow(dataWindow: DataWindow) = {
    new LabeledDataTable(dataWindow,
      _allHeaderWidths,
      _sheet,
      _sortColumn,
      sortAscending,
      rebuild = true)
  }

  def updateColumnOrder(permutations: Map[Int, Int]) = {
    val reversePermutations = permutations map (_.swap)
    val newWidths = for (i <- List.range(0, _allHeaderWidths.length)) yield {
      _allHeaderWidths(reversePermutations.getOrElse(i, i))
    }
    new LabeledDataTable(_dataWindow,
      newWidths,
      _sheet,
      _sortColumn,
      sortAscending,
      rebuild = true)
  }

  def resizeColumn(columnIndex: Int, width: Double) = {
    val realIndex = _dataWindow.windowToAbsoluteColumn(columnIndex)
    new LabeledDataTable(_dataWindow,
      _allHeaderWidths.take(realIndex) ++ List(width) ++ _allHeaderWidths.drop(realIndex + 1),
      _sheet,
      _sortColumn,
      sortAscending,
      rebuild = false)
  }

  def addNewColumn(index: Int) = {
    if (index == -1) {
      new LabeledDataTable(_dataWindow.addNewColumn(),
        _allHeaderWidths :+ DefaultProperties.COLUMN_WIDTH.toDouble,
        _sheet,
        _sortColumn,
        sortAscending,
        rebuild = true)
    }
    else {
      this // TODO: if the column is added in the middle, columns need to be reordered
    }
  }

  def addNewRow(index: Int) = {
    if (index == -1) {
      new LabeledDataTable(_dataWindow.addNewRow(),
        _allHeaderWidths,
        _sheet,
        _sortColumn,
        sortAscending,
        rebuild = true)
    }
    else {
      this // TODO: if the row is added in the middle, rows need to be reordered
    }
  }

  // the maximum horizontal and vertical offsets the window can have
  def windowMaxOffsets = (_dataWindow.dataSize._1 - _dataWindow.columnCount,
    _dataWindow.dataSize._2 - _dataWindow.rowCount)
  // the current offsets of the window
  def windowOffsets = (_dataWindow.visibleBounds._1, _dataWindow.visibleBounds._3)

}

/**
 * Object with general operations and properties of the LabeledDataTable class
 */
object LabeledDataTable {
  type DataRow = ObservableBuffer[ObjectProperty[DataCell]]
  type DataTable = ObservableBuffer[DataRow]

  def buildDataTable(rows: Int,
                     columns: Int,
                     dataGrabber: (CellPos) => DataCell): DataTable = {
    new DataTable() ++=
      List.range(0, rows).map(r => new DataRow() ++=
        List.range(0, columns).map(c =>
          ObjectProperty(dataGrabber((c, r)))))
  }

  def getHeaders(bounds: (Int, Int, Int, Int)) =
    List.range(bounds._1, bounds._2) map numToCol

  val defaultDataWindow = new DataWindow(
    (DefaultProperties.GRID_SIZE._1, DefaultProperties.GRID_SIZE._2),
    (0, DefaultProperties.GRID_SIZE._1, 0, DefaultProperties.GRID_SIZE._2))

  val defaultHeaderWidths =
    List.fill(defaultDataWindow.columnCount)(DefaultProperties.COLUMN_WIDTH.toDouble)

  def dataWithIndex(data: List[List[String]]): List[(Int, Int, String)] =
    data.zipWithIndex.flatMap({
      case (row, i) => row.zipWithIndex.map({
        case (expression, j) => (i, j, expression)
      })
    })
}
