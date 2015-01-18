package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.ObjectProperty

import scalaExcel.model.Sheet
import scalaExcel.util.DefaultProperties
import scalaExcel.GUI.data.DataWindow.{Size, Bounds}

/**
 * Wrapper on the data model sheet
 *
 * It contains all sheet data, but also information on the current window,
 * the column headers and widths, etc.
 *
 * The 'rebuild' property shows if the table has changed its structure
 * and needs to be rebuilt, or only its contents need updating
 */
class LabeledDataTable( _dataWindow: DataWindow = DataWindow.DEFAULT,
                        _allHeaderWidths: List[Double] = LabeledDataTable.DEFAULT_WIDTHS,
                        _sheet: Sheet = new scalaExcel.model.immutable.Sheet(),
                        val rebuild: Boolean,
                        _width: Double = 0,
                        _height: Double = 0) {

  def headers = _dataWindow.visibleHeaders

  def headerWidths =
    _allHeaderWidths
      .drop(_dataWindow.visibleBounds.minCol)
      .take(_dataWindow.columnCount)

  def toSheetIndex = _dataWindow.windowToAbsolute _

  def toTableIndex(index: CellPos) = {
    val tableIndex = _dataWindow.absoluteToWindow(index)
    if (_dataWindow.isInBounds(tableIndex)) Some(tableIndex)
    else None
  }

  def slideWindowBy(offsets: Bounds) =
    updateWindow(_dataWindow.slideBy(offsets))

  def slideWindowTo(bounds: Bounds) =
    updateWindow(_dataWindow.slideTo(bounds))

  /**
   * Gets the contents of the sheet cell as a DataCell
   * @param index sheet index of cell
   * @return
   */
  def dataCellFromSheet(index: CellPos) = {
    // build a DataCell with the contents of the sheet at that position
    DataCell.newEvaluated(
      _sheet.getCell(index).f,
      _sheet.getValue(index),
      _sheet.getCellStyle(index)
    )
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
    new LabeledDataTable(_dataWindow.expandTo(Size(sheet.size._1, sheet.size._2)),
      _allHeaderWidths,
      sheet,
      rebuild = false,
      _width,
      _height)
  }

  /**
   * Recalculates the desired data window's size in order to fit to the
   * available width and height, and also stores the new sizes in the
   * associated internal _width and _height properties
   * @param window      the new desired window
   * @param availableWidth  the width available to the table
   * @param availableHeight the height available to the table
   * @param bottomUpRows    specifies if the window rows should be calculated
   *                        bottom-up (from the last data row upwards)
   * @param bottomUpCols    specifies if the window columns should be
   *                        calculated bottom-up (from the last data column
   *                        upwards)
   * @return                the data table with updated window
   */
  private def updateWindow(window: DataWindow,
                           availableWidth: Double = _width,
                           availableHeight: Double = _height,
                           bottomUpRows: Boolean = false,
                           bottomUpCols: Boolean = false) = {
    val (minRow, maxRow) =
      if (bottomUpRows)
      // if bottom-up, start from the last data row and fit rows upward
        (Math.max(0, window.dataSize.rowCount - fitRows(availableHeight)),
          window.dataSize.rowCount)
      else
      // else start from the current position and fit rows downward
        (window.visibleBounds.minRow,
          Math.min(window.dataSize.rowCount,
            window.visibleBounds.minRow + fitRows(availableHeight)))

    val (minCol, maxCol) =
      if (bottomUpCols)
        // if bottom-up, start from the last data column and fit columns upward
        (Math.max(0, window.dataSize.columnCount - fitColumns(availableWidth, maxRow)),
          window.dataSize.columnCount)
      else
        // else start from the current position and fit columns downward
        (window.visibleBounds.minCol,
          Math.min(window.dataSize.columnCount,
            window.visibleBounds.minCol + fitColumns(availableWidth, maxRow)))

    // slide window to new bounds and memorize _width and _height
    new LabeledDataTable(
      window.slideTo(Bounds(minCol, maxCol, minRow, maxRow)),
      _allHeaderWidths,
      _sheet,
      rebuild = true,
      availableWidth,
      availableHeight
    )
  }

  def updateColumnOrder(permutations: Map[Int, Int]) = {
    val reversePermutations = permutations map (_.swap)
    val newWidths = for (i <- List.range(0, _allHeaderWidths.length)) yield {
      _allHeaderWidths(reversePermutations.getOrElse(i, i))
    }
    new LabeledDataTable(_dataWindow,
      newWidths,
      _sheet,
      rebuild = true,
      _width,
      _height)
  }

  def resizeColumn(columnIndex: Int, width: Double) = {
    new LabeledDataTable(_dataWindow,
      _allHeaderWidths.take(columnIndex) ++ List(width) ++ _allHeaderWidths.drop(columnIndex + 1),
      _sheet,
      rebuild = false,
      _width,
      _height)
  }

  def addColumns(count: Int, index: Int) =
    new LabeledDataTable(_dataWindow,
      // add the new columns' widths to the set
      _allHeaderWidths.take(index) ++
        List.fill(count)(DefaultProperties.COLUMN_WIDTH.toDouble) ++
        _allHeaderWidths.drop(index),
      _sheet,
      rebuild = true,
      _width,
      _height
    )
    // update the window to contain new columns
    .updateWindow(
        // expand data size
        _dataWindow.addDataColumns(count),
        // if they are added at the end, they should be in view
        bottomUpCols = index == _dataWindow.dataSize.columnCount
      )

  def addRows(count: Int, index: Int) =
    // update the window to contain new rows
    updateWindow(
      // expand data size
      _dataWindow.addDataRows(count),
      // if they are added to the end, they should be in view
      bottomUpRows = index == _dataWindow.dataSize.rowCount
    )


  def removeColumns(count: Int, index: Int) = {
    // make sure the count does not exceed data size
    val maxCount = Math.min(count, _dataWindow.dataSize.columnCount - index)
    new LabeledDataTable(_dataWindow,
      // remove the new columns' widths from the set
      _allHeaderWidths.take(index) ++ _allHeaderWidths.drop(index + maxCount),
      _sheet,
      rebuild = true,
      _width,
      _height
    )
    // update the window to remove the columns
    .updateWindow(
      // shrink data size
      _dataWindow.removeDataColumns(maxCount),
      // if the visible window is now out of bounds, slide it to the end
      bottomUpCols = _dataWindow.visibleBounds.minCol >
        _dataWindow.visibleBounds.maxCol - maxCount
    )
  }

  def removeRows(count: Int, index: Int) = {
    // make sure the count does not exceed data size
    val maxCount = Math.min(count, _dataWindow.dataSize.rowCount - index)
    // update the window to remove the rows
    updateWindow(
      // shrink data size
      _dataWindow.removeDataRows(maxCount),
      // if the visible window is now out of bounds, slide it to the end
      bottomUpRows = _dataWindow.visibleBounds.minRow >
        _dataWindow.visibleBounds.maxRow - maxCount
    )
  }

  def gridSize = _dataWindow.dataSize

  def windowSize = Size(_dataWindow.columnCount, _dataWindow.rowCount)

  /**
   * Calculates the maximum horizontal and vertical offsets the window can have
   */
  def windowMaxOffsets =
    (_dataWindow.dataSize.columnCount - _dataWindow.columnCount,
      _dataWindow.dataSize.rowCount - _dataWindow.rowCount)

  /**
   * Calculates the current offsets of the window
   */
  def windowOffsets =
    (_dataWindow.visibleBounds.minCol,
      _dataWindow.visibleBounds.minRow)

  /**
   * Returns the column width such that the number of the last row fits inside
   * the cell
   */
  def calculateColWidth(maxRow: Int = _dataWindow.visibleBounds.maxRow) =
    Math.max(20, maxRow.toString.length * DefaultProperties.NUMBERED_COLUMN_WIDTH)

  /**
   * Calculates the maximum number of data columns that fit in a given table width
   * @param availableWidth  the width available to the table
   * @return                number of columns that fit
   */
  def fitColumns(availableWidth: Double, maxRow: Int = _dataWindow.visibleBounds.maxRow) = {
    // all header widths (including numbered column)
    val widths = _allHeaderWidths.::(calculateColWidth(maxRow).toDouble)
    // number of columns that fit in the table container
    val safeWidth =  Math.max(0, availableWidth - DefaultProperties.CONTAINER_BUFFER)
    widths.scan(0.0)((acc, w) => acc + w).drop(2).takeWhile(_ < safeWidth).length
  }

  /**
   * Calculates the maximum number of data rows that fit in a given table height
   * @param availableHeight  the height available to the table
   * @return                number of rows that fit
   */
  def fitRows(availableHeight: Double) = {
    // If rows can vary height, this must be rewritten
    val rowHeight = DefaultProperties.FIXED_ROW_HEIGHT
    // number of rows that fit in the table container (-1 because of header row)
    val safeHeight =  Math.max(0, availableHeight - DefaultProperties.CONTAINER_BUFFER)
    Math.max(0, (safeHeight / rowHeight).toInt - 1)
  }

  /**
   * Uses the new available width and height to update the current data
   * window's size
   * @param availableWidth  the available width
   * @param availableHeight the available height
   * @return                the table with the updated data window
   */
  def layOut(availableWidth: Double, availableHeight: Double) =
    updateWindow(_dataWindow, availableWidth, availableHeight)

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

  val DEFAULT_WIDTHS =
    List.fill(DataWindow.DEFAULT.dataSize.columnCount)(DefaultProperties.COLUMN_WIDTH.toDouble)
}
