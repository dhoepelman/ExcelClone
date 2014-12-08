package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalaExcel.formula.VEmpty
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.ObjectProperty

import scalaExcel.model.{Sheet, Styles}
import scalaExcel.util.DefaultProperties
import scalaExcel.GUI.data.DataWindow.{Size, Bounds}
import scalaExcel.model.Filer._

/**
 * Wrapper on the data model sheet
 *
 * It contains all sheet data, but also information on the current window,
 * the column headers and widths, etc.
 *
 * The 'rebuild' property shows if the table has changed its structure
 * and needs to be rebuilt, or only its contents need updating
 */
class LabeledDataTable(
                        _dataWindow: DataWindow = DataWindow.DEFAULT,
                        _allHeaderWidths: List[Double] = LabeledDataTable.DEFAULT_WIDTHS,
                        _sheet: Sheet = new Sheet(),
                        val rebuild: Boolean) {

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
      rebuild = false)
  }

  private def updateWindow(dataWindow: DataWindow) = {
    new LabeledDataTable(dataWindow,
      _allHeaderWidths,
      _sheet,
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
      rebuild = true)
  }

  def resizeColumn(columnIndex: Int, width: Double) = {
    new LabeledDataTable(_dataWindow,
      _allHeaderWidths.take(columnIndex) ++ List(width) ++ _allHeaderWidths.drop(columnIndex + 1),
      _sheet,
      rebuild = false)
  }

  def addNewColumn(index: Int) = {
    if (index == -1) {
      new LabeledDataTable(_dataWindow.addNewColumn(),
        _allHeaderWidths :+ DefaultProperties.COLUMN_WIDTH.toDouble,
        _sheet,
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
        rebuild = true)
    }
    else {
      this // TODO: if the row is added in the middle, rows need to be reordered
    }
  }

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
   * Calculates the maximum number of data columns that fit in a given table width
   * @param availableWidth  the width available to the table
   * @return                number of columns that fit
   */
  def fitColumns(availableWidth: Double) = {
    // all header widths (including numbered column)
    val widths = _allHeaderWidths.::(DefaultProperties.NUMBERED_COLUMN_WIDTH.toDouble)
    // number of columns that fit in the table container
    val cols = widths.scan(0.0)((acc, w) => acc + w).drop(2).takeWhile(_ < availableWidth).length
    // truncate the number at maximum column number (if applicable)
    Math.min(cols, _dataWindow.dataSize.columnCount)
  }

  /**
   * Calculates the maximum number of data rows that fit in a given table height
   * @param availableHeight  the height available to the table
   * @return                number of rows that fit
   */
  def fitRows(availableHeight: Double) = {
    // TODO if rows can vary height, this must be rewritten
    val rowHeight = DefaultProperties.FIXED_ROW_HEIGHT
    // number of rows that fit in the table container (-1 because of header row)
    val rows = Math.max(0, (availableHeight / rowHeight).toInt - 1)
    // truncate the number of rows at maximum row number (if applicable)
    Math.min(rows, _dataWindow.dataSize.rowCount)
  }

  /**
   * Recalculates the data window to fit all columns and rows that can be seen
   * @param availableWidth  the width available to the table
   * @param availableHeight the height available to the table
   * @return
   */
  def layOut(availableWidth: Double, availableHeight: Double) = {
    // move window to initial position, but with new size
    slideWindowTo(Bounds(0, fitColumns(availableWidth),0, fitRows(availableHeight)))
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

  val DEFAULT_WIDTHS =
    List.fill(DataWindow.DEFAULT.dataSize.columnCount)(DefaultProperties.COLUMN_WIDTH.toDouble)
}
