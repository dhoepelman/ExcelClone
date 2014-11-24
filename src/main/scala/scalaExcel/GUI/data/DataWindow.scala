package scalaExcel.GUI.data

class DataWindow(val maxBounds: (Int, Int, Int, Int),
                 val visibleBounds: (Int, Int, Int, Int)) {

  def windowToAbsoluteColumn(colIndex: Int) =
    colIndex + visibleBounds._3

  def absoluteToWindowColumn(colIndex: Int) =
    colIndex - visibleBounds._3

  def windowToAbsoluteRow(rowIndex: Int) =
    rowIndex + visibleBounds._1

  def absoluteToWindowRow(rowIndex: Int) =
    rowIndex - visibleBounds._1

  def windowToAbsolute(index: (Int, Int)): (Int, Int) =
    (windowToAbsoluteRow(index._1), windowToAbsoluteColumn(index._2))

  def absoluteToWindow(index: (Int, Int)): (Int, Int) =
    (absoluteToWindowRow(index._1), absoluteToWindowColumn(index._2))

  def isInBounds(index: (Int, Int)) = {
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
    new DataWindow(maxBounds,
      bounds)
  }

  def slideTo(bounds: (Int, Int, Int, Int)) = {
    new DataWindow(maxBounds,
      bounds)
  }

  def columnCount = visibleBounds._2 - visibleBounds._1

  def rowCount = visibleBounds._4 - visibleBounds._3
}
