package scalaExcel.GUI.data

import scala.math.max

import scalaExcel.CellPos
import scalaExcel.GUI.data.DataWindow._
import scalaExcel.util.DefaultProperties
import scalaExcel.formula.numToCol

class DataWindow(val dataSize: Size,
                 val visibleBounds: Bounds) {


  def windowToAbsolute(index: CellPos) =
    (index._1 + visibleBounds.minCol, index._2 + visibleBounds.minRow)

  def absoluteToWindow(index: CellPos) =
    (index._1 - visibleBounds.minCol, index._2 - visibleBounds.minRow)

  def isInBounds(index: CellPos) = visibleBounds.includes(index)

  def slideBy(offsets: Bounds) = {
    new DataWindow(dataSize, offsets.add(visibleBounds))
  }

  def slideTo(bounds: Bounds) = new DataWindow(dataSize, bounds)

  def columnCount = visibleBounds.maxCol - visibleBounds.minCol

  def rowCount = visibleBounds.maxRow - visibleBounds.minRow

  def addNewRows(count: Int, inView: Boolean) =
    new DataWindow(
      // add count rows to maximum bounds
      Size(dataSize.columnCount, dataSize.rowCount + count),
      // if they should be in view, slide the visibleBounds over them
      if (inView)
        visibleBounds.add(Bounds(0, 0, count, count))
      else
        visibleBounds)

  def addNewColumns(count: Int, inView: Boolean) =
    new DataWindow(
      // add count columns to maximum bounds
      Size(dataSize.columnCount + count, dataSize.rowCount),
      // if they should be in view, slide the visibleBounds over them
      if (inView)
        visibleBounds.add(Bounds(count, count, 0, 0))
      else
        visibleBounds)

  def removeRows(count: Int) = {
    val rowNum = dataSize.rowCount - count
    val offset = Math.min(visibleBounds.maxRow,  rowNum) - visibleBounds.maxRow
    new DataWindow(
      // remove count rows from maximum bounds
      Size(dataSize.columnCount, rowNum),
      // if the rows were in view, slide the visibleBounds before them
      visibleBounds.add(Bounds(0, 0, offset, offset)))
  }

  def removeColumns(count: Int) = {
    val colNum = dataSize.columnCount - count
    val offset = Math.min(visibleBounds.maxCol, colNum) - visibleBounds.maxCol
    new DataWindow(
      // remove count columns from maximum bounds
      Size(dataSize.columnCount - count, dataSize.rowCount),
      // if the columns were in view, slide the visibleBounds before them
      visibleBounds.add(Bounds(offset, offset, 0, 0)))
  }

  def expandTo(size: Size) =
    new DataWindow(
      Size(
        max(size.columnCount, dataSize.columnCount),
        max(size.rowCount, dataSize.rowCount)),
      visibleBounds
    )

  def visibleHeaders =
    List.range(visibleBounds.minCol, visibleBounds.maxCol) map numToCol

}

object DataWindow {

  case class Bounds(minCol: Int, maxCol: Int, minRow: Int, maxRow: Int) {
    def add(b2: Bounds) =
      Bounds(minCol + b2.minCol,
             maxCol + b2.maxCol,
             minRow + b2.minRow,
             maxRow + b2.maxRow)

    def includes(pos: CellPos) =
      pos._1 >= minCol && pos._1 < maxCol &&
        pos._2 >= minRow && pos._2 < maxRow

  }

  case class Size(columnCount: Int, rowCount: Int)

  val DEFAULT = new DataWindow(
    Size(DefaultProperties.GRID_SIZE._1, DefaultProperties.GRID_SIZE._2),
    Bounds(0, DefaultProperties.GRID_SIZE._1, 0, DefaultProperties.GRID_SIZE._2)
  )

}
