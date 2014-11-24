package scalaExcel.GUI.data

class DataWindow(val maxBounds: (Int, Int, Int, Int),
                 val visibleBounds: (Int, Int, Int, Int),
                 _columnHeaders: List[String],
                 _columnWidths: List[Double],
                 _columnPermutations: Map[Int, Int],
                 _rowPermutations: Map[Int, Int]) {

  def windowToAbsoluteColumn(colIndex: Int) =
    _columnPermutations.map(_.swap).getOrElse(colIndex + visibleBounds._3, colIndex + visibleBounds._3)

  def absoluteToWindowColumn(colIndex: Int) =
    _columnPermutations.getOrElse(colIndex, colIndex) - visibleBounds._3

  def windowToAbsoluteRow(rowIndex: Int) =
    _rowPermutations.map(_.swap).getOrElse(rowIndex + visibleBounds._1, rowIndex + visibleBounds._1)

  def absoluteToWindowRow(rowIndex: Int) =
    _rowPermutations.getOrElse(rowIndex, rowIndex) - visibleBounds._1

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

  private def applyRestrictions[T: Manifest](list: List[T]) = {
    val reversed = _columnPermutations.map(_.swap)
    list.zipWithIndex.map(indexedCol =>
      reversed.get(indexedCol._2) match {
        case Some(index) => list(index)
        case _ => indexedCol._1
      }).drop(visibleBounds._3).take(columnCount)
  }

  def columnHeaders: List[String] =
    applyRestrictions(_columnHeaders)

  def columnWidths: List[Double] =
    applyRestrictions(_columnWidths)

  def slideBy(offsets: (Int, Int, Int, Int)) = {
    val bounds = (visibleBounds._1 + offsets._1,
      visibleBounds._2 + offsets._2,
      visibleBounds._3 + offsets._3,
      visibleBounds._4 + offsets._4)
    new DataWindow(maxBounds,
      bounds,
      _columnHeaders,
      _columnWidths,
      _columnPermutations,
      _rowPermutations)
  }

  def slideTo(bounds: (Int, Int, Int, Int)) = {
    new DataWindow(maxBounds,
      bounds,
      _columnHeaders,
      _columnWidths,
      _columnPermutations,
      _rowPermutations)
  }

  private def combinePermutations(oldOrder: Map[Int, Int], newOrder: Map[Int, Int]) = {
    val modifiedPermutations = oldOrder.map(pair => (pair._1, newOrder.getOrElse(pair._2, pair._2)))
    val newPermutations = newOrder.filter(pair => !oldOrder.keySet.contains(pair._1))
    modifiedPermutations ++ newPermutations
  }

  def reorderColumns(permutations: Map[Int, Int]) = {
    new DataWindow(maxBounds,
      visibleBounds,
      _columnHeaders,
      _columnWidths,
      combinePermutations(_columnPermutations, permutations),
      _rowPermutations)
  }

  def reorderRows(permutations: Map[Int, Int]) = {
    new DataWindow(maxBounds,
      visibleBounds,
      _columnHeaders,
      _columnWidths,
      _columnPermutations,
      permutations)
  }

  def columnCount = visibleBounds._2 - visibleBounds._1

  def rowCount = visibleBounds._4 - visibleBounds._3
}

object DataWindowTester {
  def main(args: Array[String]) {
    val window = new DataWindow((0, 4, 0, 4),
      (0, 4, 0, 4),
      List("A", "B", "C", "D", "E", "F"),
      List(100, 200, 100, 100, 100, 100),
      Map(1 -> 0, 2 -> 1, 0 -> 2),
      Map())
    println(window.reorderColumns(Map()).columnHeaders)
  }
}
