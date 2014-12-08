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

  def addNewRow() =
    new DataWindow(
      // add one more column to maximum bounds
      Size(dataSize.columnCount, dataSize.rowCount + 1),
      // if the column should be in view, slide the visibleBounds over it
      if (visibleBounds.maxRow == dataSize.rowCount)
        (Bounds(0, 0, 1, 1) add visibleBounds)
      else
        visibleBounds
    )

  def addNewColumn() =
    new DataWindow(
      // add one more column to maximum bounds
      Size(dataSize.columnCount + 1, dataSize.rowCount),
      // if the row should be in view, slide the visibleBounds over it
      if (visibleBounds.maxCol == dataSize.columnCount)
        (Bounds(1, 1, 0, 0) add visibleBounds)
      else
        visibleBounds)

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

  case class Bounds(
      val minCol: Int,
      val maxCol: Int,
      val minRow: Int,
      val maxRow: Int) {

    def add(b2: Bounds) =
      Bounds(minCol + b2.minCol,
             maxCol + b2.maxCol,
             minRow + b2.minRow,
             maxRow + b2.maxRow)

    def includes(pos: CellPos) =
      pos._1 >= minCol && pos._1 < maxCol &&
        pos._2 >= minRow && pos._2 < maxRow

  }

  case class Size(val columnCount: Int, val rowCount: Int)

  val DEFAULT = new DataWindow(
    Size(DefaultProperties.GRID_SIZE._1, DefaultProperties.GRID_SIZE._2),
    Bounds(0, DefaultProperties.GRID_SIZE._1, 0, DefaultProperties.GRID_SIZE._2)
  )

}
