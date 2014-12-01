package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalaExcel.GUI.data.DataWindow._

class DataWindow(val maxBounds: (MinX, MaxX, MinY, MaxY),
                 val visibleBounds: (MinX, MaxX, MinY, MaxY)) {

  def addToVisibleBound(f: (Bounds) => Int, add: (Int, Int) => Int)(x: Int) = add(x, f(visibleBounds))

  def windowToAbsoluteColumn = addToVisibleBound(_._3, _ + _) _
  def absoluteToWindowColumn = addToVisibleBound(_._3, _ - _) _
  def windowToAbsoluteRow = addToVisibleBound(_._1, _ + _) _
  def absoluteToWindowRow = addToVisibleBound(_._1, _ - _) _

  def windowToAbsolute(index: (Int, Int)): (Int, Int) =
    (windowToAbsoluteRow(index._1), windowToAbsoluteColumn(index._2))

  def absoluteToWindow(index: (Int, Int)): (Int, Int) =
    (absoluteToWindowRow(index._1), absoluteToWindowColumn(index._2))

  def isInBounds(index: CellPos) = {
    if (index._1 >= visibleBounds._1 && index._1 < visibleBounds._2 &&
        index._2 >= visibleBounds._3 && index._2 < visibleBounds._4)
      true
    else
      false
  }

  def slideBy(offsets: (Int, Int, Int, Int)) = {
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

  lazy val visibleIndexes =
    List.range(visibleBounds._1, visibleBounds._2).flatMap(c =>
      List.range(visibleBounds._3, visibleBounds._4).map(r => (c, r)))

}

object DataWindow {
  type MinX = Int
  type MaxX = Int
  type MinY = Int
  type MaxY = Int

  type Bounds = (MinX, MaxX, MinY, MaxY)
}