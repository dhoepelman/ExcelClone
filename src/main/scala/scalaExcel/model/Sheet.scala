package scalaExcel.model

import scalafx.scene.paint.Color
import scalaExcel.formula._

/**
 * Sheet is currently the entire immutable datamodel
 * @param cells objects of the cell Class, that know how to execute themselves
 * @param values final values of all positions in the sheet
 * @param dependents a map lists of cells that depend on that cell
 */
class Sheet(val cells: Map[CellPos, Cell] = Map(),
            val values: Map[CellPos, Value] = Map(),
            val dependents: Map[CellPos, List[CellPos]] = Map(),
            val styles: Map[CellPos, Styles] = Map()) {

  /**
   * Set the cell at (x,y) to some formula f
   * @return the new sheet, and a list of cells that need to be recalculated
   */
  def setCell(x: Int, y: Int, f: String) : (Sheet, List[CellPos]) = setCell((x,y), f)

  /**
   * Set the cell at pos to some formula f
   * @return the new sheet, and a list of cells that need to be recalculated
   */
  def setCell(pos : CellPos, f : String) : (Sheet, List[CellPos]) = {
    val newCell = Cell(pos, f)
    setCell(pos, newCell)
  }

  private def setCell(pos: CellPos, newCell : Cell) = {
    val newCells = cells + (newCell.position -> newCell)
    val newValues = calcNewValue(newCell)
    val newDependents = calcNewDependents(newCell)
    (new Sheet(newCells, newValues, newDependents, styles), dependentsOf(newCell))
  }

  def deleteCell(p : CellPos) = {
    (new Sheet(cells - p, values - p, dependents - p, styles - p), dependentsOf(p))
  }

  /**
   * Copy a cell and change its dependencies relative to the new position
   */
  def copyCell(from: CellPos, to: CellPos) = {
    val cell = Cell(to, DependencyModifier.moveDependencies(from, to)(getCell(from).AST))
    (new Sheet(cells + (to -> cell), calcNewValue(cell), calcNewDependents(cell), styles), dependentsOf(cell))
  }

  /**
   * Cut a cell to a new location and propagate the location change to its dependents.
   */
  def cutCell(from: CellPos, to: CellPos) = {
    val dependents = dependentsOf(from)
    val toUpdate = dependents ++ dependentsOf(to)

    val (tempSheet, _) = setCell(to, Cell(to, getCell(from)))

    // Change all the dependent cells to point to the new cell
    val tempSheet2 = dependents.foldLeft(tempSheet){(s, pos) =>
      s.setCell(pos, Cell(pos, DependencyModifier.changeDependency(from, to)(s.getCell(pos).AST)))._1
    }

    // Delete the original cell
    val (newSheet, _) = tempSheet2.deleteCell(from)

    (newSheet, toUpdate)
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

  /** Set the style of a cell */
  def setCellStyle(x: Int, y: Int, s: Styles) = {
    new Sheet(cells, values, dependents, styles + ((x, y) -> s))
  }

  /**
   * Set a cell to the circular reference error
   */
  def setToCircular(x: Int, y: Int) = {
    new Sheet(cells, values + ((x,y) -> VErr(CircularRef)), dependents)
  }

  /** Get the cells that depend on this given cell */
  def dependentsOf(c: Cell) : List[CellPos] = dependentsOf(c.position)
  def dependentsOf(p: CellPos) : List[CellPos] = dependents getOrElse(p, List())

  def valueAt(x: Int, y: Int) = values get ((x, y))

  /** Get the Cell or return an empty cell */
  def getCell(x : Int, y : Int) : Cell = getCell((x,y))

  /** Get the Cell or return an empty cell */
  def getCell(pos : CellPos) : Cell = cells getOrElse(pos, Cell(pos))

  private def calcNewValue(c: Cell) = {
    val value = c.eval(values)
    values + (c.position -> value)
  }

  private def calcNewDependents(c: Cell) = {
    val oldCell = getCell(c.position)
    val oldDeps = oldCell.refs

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
