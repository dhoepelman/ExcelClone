package scalaExcel.model

import scalaExcel.formula.Value

// A Sheet is currently the entire immutable datamodel, containing:
// - the cells, objects of the cell Class, that know how to execute themselves
// - final values of all positions in the sheet
// - a map lists of cells that depend on that cell
class Sheet(val cells: Map[(Int, Int), Cell] = Map(),
            val values: Map[(Int, Int), Value] = Map(),
            val dependents: Map[(Int, Int), List[(Int, Int)]] = Map()) {

  // Set the cell at (x,y) to some formula f, return the new sheet, and a list
  // of cells that need to be recalculated
  def setCell(x: Int, y: Int, f: String) = {
    val newCell = new Cell(x, y, f)
    val newCells = cells + (newCell.position -> newCell)
    val newValues = calcNewValue(newCell)
    val newRefs = calcNewRefs(newCell)
    (new Sheet(newCells, newValues, newRefs), dependentsOf(newCell))
  }

  // recalculate the value of a cell, return a new sheet which includes the new
  // value, and a list of cells that also need to be updated
  def updateCell(x: Int, y: Int) = {
    cells get ((x, y)) match {
      case Some(c) => (new Sheet(cells, calcNewValue(c), dependents), dependentsOf(c))
      case None => (this, List[(Int, Int)]())
    }
  }

  // Get the cells that depend on this given cell
  def dependentsOf(c: Cell) = dependents get c.position match {
    case Some(l) => l
    case None => List()
  }

  def valueAt(x: Int, y: Int) = values get ((x, y))

  private def calcNewValue(c: Cell) = {
    val value = c.eval(values)
    values + (c.position -> value)
  }

  private def calcNewRefs(c: Cell) = {
    val oldCell = cells get (c.position)
    val oldRefs = oldCell match {
      case Some(c) => c.refs
      case None => List()
    }

    val r = c.refs
    val rmvRefs = oldRefs diff r
    val addRefs = r diff oldRefs

    val newRefsA = rmvRefs.foldLeft(dependents)((refsMap, ref) => refsMap get ref match {
      case Some(l) => refsMap + (ref -> (l diff List(c.position)))
      case None => refsMap
    })

    addRefs.foldLeft(newRefsA)((refsMap, ref) => refsMap get ref match {
      case Some(l) => refsMap + (ref -> (l :+ c.position))
      case None => refsMap + (ref -> List(c.position))
    })
  }

  override def toString = (
    cells.toString(),
    dependents.toString(),
    values.toString()
    ).toString()

}
