package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalaExcel.formula.{VEmpty, numToCol}
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.ObjectProperty

import scalaExcel.model.{Sheet, Styles}
import scalaExcel.util.DefaultProperties
import scalaExcel.GUI.data.DataWindow.{Bounds, Offsets}
import scalaExcel.model.Filer._

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

  def toTableIndex(index: CellPos) = {
    val tableIndex = _dataWindow.absoluteToWindow(index)
    if(_dataWindow.isInBounds(tableIndex)) tableIndex
    else null
  }

  def slideWindowBy(offsets: Offsets) =
    updateWindow(_dataWindow.slideBy(offsets))

  def slideWindowTo(bounds: Bounds) =
    updateWindow(_dataWindow.slideTo(bounds))

  /**
   * Gets the contents of the sheet cell as a DataCell
   * @param index sheet index of cell
   * @return
   */
  def dataCellFromSheet(index: CellPos) = {
   
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

  /**
   * Gets the contents of the sheet cell as a DataCell
   * @param index table index of cell
   * @return
   */
  def dataCellFromTable(index: CellPos) = dataCellFromSheet(toSheetIndex(index))

  def data = LabeledDataTable.buildDataTable(
      _dataWindow.rowCount,
      _dataWindow.columnCount,
      dataCellFromTable)

  def updateContents(sheet: Sheet) = {
    new LabeledDataTable(_dataWindow.expandTo(sheet.size),
      _allHeaderWidths,
      sheet,
      _sortColumn,
      sortAscending,
      rebuild = false)
  }

  private def updateWindow(dataWindow: DataWindow) = {
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

  /**
   * Calculates the maximum horizontal and vertical offsets the window can have
   */
  def windowMaxOffsets = (_dataWindow.dataSize._1 - _dataWindow.columnCount,
    _dataWindow.dataSize._2 - _dataWindow.rowCount)
  
  /**
   * Calculates the current offsets of the window
   */
  def windowOffsets = (_dataWindow.visibleBounds._1, _dataWindow.visibleBounds._3)

  /**
   * Recalculates the data window to fit all columns and rows that can be seen
   * @param tableWidth  the width available to the table
   * @param tableHeight the height available to the table
   * @param rowHeight   the current row height
   * @return
   */
  def layOut(tableWidth: Double, tableHeight: Double, rowHeight: Double) = {
    // TODO if rows can vary height, this must be rewritten
    val realRowHeight = if (rowHeight < 0) DefaultProperties.FIXED_ROW_HEIGHT else rowHeight
    // number of rows that fit in the table container (-1 because of header row)
    val rows = Math.max(0, (tableHeight / realRowHeight).toInt - 1)
    // all header widths (including numbered column)
    val widths = _allHeaderWidths.::(DefaultProperties.NUMBERED_COLUMN_WIDTH.toDouble)
    // number of columns that fit in the table container
    val cols = widths.scan(0.0)((acc, w) => acc + w).drop(2).takeWhile(_ < tableWidth).length
    // truncate the number at maximum column number (if applicable)
    val maxCols = Math.min(cols, _dataWindow.dataSize._1)
    // truncate the number of rows at maximum row number (if applicable)
    val maxRows = Math.min(rows, _dataWindow.dataSize._2)
    // move window to initial position, but with new size
    slideWindowTo((0, maxCols,0, maxRows))
  }

  def saveTo(file: java.io.File) = _sheet.saveTo(file)

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
    List.fill(defaultDataWindow.dataSize._1)(DefaultProperties.COLUMN_WIDTH.toDouble)

  def dataWithIndex(data: List[List[String]]): List[(Int, Int, String)] =
    data.zipWithIndex.flatMap({
      case (row, i) => row.zipWithIndex.map({
        case (expression, j) => (i, j, expression)
      })
    })
}
