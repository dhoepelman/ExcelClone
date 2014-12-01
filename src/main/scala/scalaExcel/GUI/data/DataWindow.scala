package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalaExcel.GUI.data.DataWindow._

class DataWindow(val maxBounds: Bounds,
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

  def isInBounds(index: CellPos) = {
    if (index._1 >= visibleBounds._1 && index._1 < visibleBounds._2 &&
        index._2 >= visibleBounds._3 && index._2 < visibleBounds._4)
      true
    else
      false
  }

  def slideBy(offsets: Offsets) = {
    val bounds = (visibleBounds._1 + offsets._1,
      visibleBounds._2 + offsets._2,
      visibleBounds._3 + offsets._3,
      visibleBounds._4 + offsets._4)
    new DataWindow(maxBounds, bounds)
  }

  def slideTo(bounds: (Int, Int, Int, Int)) = {
    new DataWindow(maxBounds, bounds)
  }

  def columnCount = visibleBounds._2 - visibleBounds._1

  def rowCount = visibleBounds._4 - visibleBounds._3

}

object DataWindow {
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
}