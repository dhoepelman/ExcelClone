package scalaExcel.model

import scalafx.scene.paint.Color
import scalaExcel.formula.{CircularRef, VErr, Value}

/**
 * Sheet is currently the entire immutable datamodel
 * @param cells objects of the cell Class, that know how to execute themselves
 * @param values final values of all positions in the sheet
 * @param dependents a map lists of cells that depend on that cell
 */
class Sheet(val cells: Map[(Int, Int), Cell] = Map(),
            val values: Map[(Int, Int), Value] = Map(),
            val dependents: Map[(Int, Int), List[(Int, Int)]] = Map(),
            val styles: Map[(Int, Int), Styles] = Map()) {

  /**
   * Set the cell at (x,y) to some formula f
   * @return the new sheet, and a list of cells that need to be recalculated
   */
  def setCell(x: Int, y: Int, f: String) = {
    val newCell = new Cell(x, y, f)
    val newCells = cells + (newCell.position -> newCell)
    val newValues = calcNewValue(newCell)
    val newDependents = calcNewDependents(newCell)
    (new Sheet(newCells, newValues, newDependents, styles), dependentsOf(newCell))
  }

  /**
   * recalculate the value of a cell
   * @return a new sheet which includes the new value, and a list of cells that also need to be updated
   */
  def updateCell(x: Int, y: Int) = {
    cells get ((x, y)) match {
      case Some(c) => (new Sheet(cells, calcNewValue(c), dependents, styles), dependentsOf(c))
      case None => (this, List[(Int, Int)]())
    }
  }

  def setCellColor(x: Int, y: Int, c: Color) = {
    val style = styles get ((x, y)) getOrElse(Styles.DEFAULT)
    new Sheet(cells, values, dependents, styles + ((x, y) -> style.setColor(c)))
  }

  /**
   * Set a cell to the circular reference error
   */
  def setToCircular(x: Int, y: Int) = {
    new Sheet(cells, values + ((x,y) -> VErr(CircularRef)), dependents)
  }

  /** Get the cells that depend on this given cell */
  def dependentsOf(c: Cell) = dependents get c.position match {
    case Some(l) => l
    case None => List()
  }

  def valueAt(x: Int, y: Int) = values get ((x, y))

  private def calcNewValue(c: Cell) = {
    val value = c.eval(values)
    values + (c.position -> value)
  }

  private def calcNewDependents(c: Cell) = {
    val oldCell = cells get (c.position)
    val oldDeps = oldCell match {
      case Some(c) => c.refs
      case None => List()
    }

    val r = c.refs
    val rmvDeps = oldDeps diff r
    val addDeps = r diff oldDeps

    val newDepsA = rmvDeps.foldLeft(dependents)((refsMap, ref) => refsMap get ref match {
      case Some(l) => refsMap + (ref -> (l diff List(c.position)))
      case None => refsMap
    })

    addDeps.foldLeft(newDepsA)((refsMap, ref) => refsMap get ref match {
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
