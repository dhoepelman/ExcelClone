package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalaExcel.GUI.data.DataWindow._
import scalaExcel.util.DefaultProperties
import scalaExcel.formula.numToCol

class DataWindow(val dataSize: Size,
                 val visibleBounds: Bounds) {

  def addToVisibleBound(f: (Bounds) => Int, add: (Int, Int) => Int)(x: Int) = add(x, f(visibleBounds))

  def windowToAbsoluteColumn = addToVisibleBound(_._1, _ + _) _
  def absoluteToWindowColumn = addToVisibleBound(_._1, _ - _) _
  def windowToAbsoluteRow = addToVisibleBound(_._3, _ + _) _
  def absoluteToWindowRow = addToVisibleBound(_._3, _ - _) _

  def windowToAbsolute(index: (Int, Int)): (Int, Int) =
    (windowToAbsoluteColumn(index._1), windowToAbsoluteRow(index._2))

  def absoluteToWindow(index: (Int, Int)): (Int, Int) =
    (absoluteToWindowColumn(index._1), absoluteToWindowRow(index._2))

  def isInBounds(index: CellPos) =
    index._1 >= visibleBounds._1 && index._1 < visibleBounds._2 &&
        index._2 >= visibleBounds._3 && index._2 < visibleBounds._4

  def slideBy(offsets: Offsets) = {
    new DataWindow(dataSize, addOffsetsToBounds(offsets, visibleBounds))
  }

  def addOffsetsToBounds(offsets: Offsets, bounds: Bounds) = {
    (visibleBounds._1 + offsets._1,
      visibleBounds._2 + offsets._2,
      visibleBounds._3 + offsets._3,
      visibleBounds._4 + offsets._4)
  }

  def slideTo(bounds: (Int, Int, Int, Int)) = {
    new DataWindow(dataSize, bounds)
  }

  def columnCount = visibleBounds._2 - visibleBounds._1

  def rowCount = visibleBounds._4 - visibleBounds._3

  def addNewRow() = {
    // add one more column to maximum bounds
    val newSize = (dataSize._1, dataSize._2 + 1)
    // if the column should be in view, slide the visibleBounds over it
    if(visibleBounds._4 == dataSize._2)
      new DataWindow(newSize, addOffsetsToBounds((0, 0, 1, 1), visibleBounds))
    else
      new DataWindow(newSize, visibleBounds)
  }

  def addNewColumn() = {
    // add one more row to maximum bounds
    val newSize = (dataSize._1 + 1, dataSize._2)
    // if the column should be in view, slide the visibleBounds over it
    if(visibleBounds._2 == dataSize._1)
      new DataWindow(newSize, addOffsetsToBounds((1, 1, 0, 0), visibleBounds))
    else
      new DataWindow(newSize, visibleBounds)
  }

  def expandTo(size: Size) =
    new DataWindow((Math.max(size._1, dataSize._1), Math.max(size._2, dataSize._2)), visibleBounds)

  def visibleHeaders =
    List.range(visibleBounds._1, visibleBounds._2) map numToCol

}

object DataWindow {
  type Cols = Int
  type Rows = Int
  type MinCol = Int
  type MinColOffset = Int
  type MaxCol = Int
  type MaxColOffset = Int
  type MinRow = Int
  type MinRowOffset = Int
  type MaxRow = Int
  type MaxRowOffset = Int

  type Bounds = (MinCol, MaxCol, MinRow, MaxRow)
  type Offsets = (MinColOffset, MaxColOffset, MinRowOffset, MaxRowOffset)
  type Size = (Cols, Rows)

  val DEFAULT = new DataWindow(
    (DefaultProperties.GRID_SIZE._1, DefaultProperties.GRID_SIZE._2),
    (0, DefaultProperties.GRID_SIZE._1, 0, DefaultProperties.GRID_SIZE._2))
}