package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalaExcel.GUI.data.DataWindow._
import scalaExcel.util.DefaultProperties
import scalaExcel.formula.numToCol

class DataWindow(val dataSize: Size,
                 val visibleBounds: Bounds) {


  def windowToAbsolute(index: CellPos) =
    (index._1 + visibleBounds.minColOffset, index._2 + visibleBounds.minRowOffset)

  def absoluteToWindow(index: CellPos) =
    (index._1 - visibleBounds.minColOffset, index._2 - visibleBounds.minRowOffset)

  def isInBounds(index: CellPos) = visibleBounds match {
    case Bounds(minCol, maxCol, minRow, maxRow) =>
      index._1 >= minCol && index._1 < maxCol &&
        index._2 >= minRow && index._2 < maxRow
  }

  def slideBy(offsets: Bounds) = {
    new DataWindow(dataSize, addBounds(offsets, visibleBounds))
  }

  def addBounds(b1: Bounds, b2: Bounds) =
    new Bounds(b1.minColOffset + b2.minColOffset,
      b1.maxColOffset + b2.maxColOffset,
      b1.minRowOffset + b2.minRowOffset,
      b1.maxRowOffset + b2.maxRowOffset)

  def slideTo(bounds: Bounds) = {
    new DataWindow(dataSize, bounds)
  }

  def columnCount =
    visibleBounds.maxColOffset - visibleBounds.minColOffset

  def rowCount =
    visibleBounds.maxRowOffset - visibleBounds.minRowOffset

  def addNewRow() =
    new DataWindow(
      // add one more column to maximum bounds
      new Size(dataSize.columnCount, dataSize.rowCount + 1),
      // if the column should be in view, slide the visibleBounds over it
      if (visibleBounds.maxRowOffset == dataSize.rowCount)
        addBounds(new Bounds(0, 0, 1, 1), visibleBounds)
      else
        visibleBounds
    )

  def addNewColumn() =
    new DataWindow(
      // add one more column to maximum bounds
      new Size(dataSize.columnCount + 1, dataSize.rowCount),
      // if the row should be in view, slide the visibleBounds over it
      if (visibleBounds.maxColOffset == dataSize.columnCount)
        addBounds(new Bounds(1, 1, 0, 0), visibleBounds)
      else
        visibleBounds)

  def expandTo(size: Size) =
    new DataWindow(
      new Size(Math.max(size.columnCount, dataSize.columnCount),
        Math.max(size.rowCount, dataSize.rowCount)
      ),
      visibleBounds
    )

  def visibleHeaders =
    List.range(visibleBounds.minColOffset, visibleBounds.maxColOffset) map numToCol

}

object DataWindow {

  case class Bounds(minColOffset: Int, maxColOffset: Int, minRowOffset: Int, maxRowOffset: Int)
  case class Size(columnCount: Int, rowCount: Int)

  val DEFAULT = new DataWindow(
    new Size(DefaultProperties.GRID_SIZE._1, DefaultProperties.GRID_SIZE._2),
    new Bounds(0, DefaultProperties.GRID_SIZE._1, 0, DefaultProperties.GRID_SIZE._2)
  )
}