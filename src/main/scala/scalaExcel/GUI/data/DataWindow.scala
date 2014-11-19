package scalaExcel.GUI.data

class DataWindow(val maxBounds: (Int, Int, Int, Int),
                 val visibleBounds: (Int, Int, Int, Int),
                 _columnHeaders: List[String],
                 _columnWidths: List[Double],
                 _columnPermutations: Map[Int, Int]) {

  def windowToAbsolute(index: (Int, Int)): (Int, Int) =
    (index._1 + visibleBounds._1, _columnPermutations.getOrElse(index._2, index._2) + visibleBounds._3)

  def absoluteToWindow(index: (Int, Int)): (Int, Int) =
    (index._1 - visibleBounds._1, _columnPermutations.getOrElse(index._2, index._2) - visibleBounds._3)

  private def applyRestrictions[T: Manifest](list: List[T]) = {
    list.zipWithIndex.map(indexedCol =>
      _columnPermutations.get(indexedCol._2) match {
        case Some(index) => list(index)
        case _ => indexedCol._1
      }).drop(visibleBounds._3).take(columnCount)
  }

  def columnHeaders: List[String] =
    applyRestrictions(_columnHeaders)

  def columnWidths: List[Double] =
    applyRestrictions(_columnWidths)

  def slide(offsets: (Int, Int, Int, Int)) = {
    val bounds = (visibleBounds._1 + offsets._1,
      visibleBounds._2 + offsets._2,
      visibleBounds._3 + offsets._3,
      visibleBounds._4 + offsets._4)
    new DataWindow(maxBounds, bounds, _columnHeaders, _columnWidths, _columnPermutations)
  }

  def reorderColumns(permutations: Map[Int, Int]) = {
    val modifiedPermutations = _columnPermutations.map(pair => (pair._1, permutations.getOrElse(pair._2, pair._2)))
    val newPermutations = permutations.filter(pair => !_columnPermutations.keySet.contains(pair._1))
    new DataWindow(maxBounds, visibleBounds, _columnHeaders, _columnWidths, modifiedPermutations ++ newPermutations)
  }

  def columnCount = visibleBounds._2 - visibleBounds._1
  def rowCount  = visibleBounds._4 - visibleBounds._3

}

object DataWindowTester {
  def main(args: Array[String]) {
    val window = new DataWindow((0, 4, 0, 4), (1, 3, 1, 3), List("A", "B", "C", "D", "E", "F"), List(100, 200, 100, 100, 100, 100), Map(1 ->3, 3->2,2 -> 1))
    println(window.reorderColumns(Map(3->4, 4->3, 0->5, 5->0)).columnHeaders)
  }
}
