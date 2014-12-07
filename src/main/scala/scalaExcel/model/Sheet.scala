package scalaExcel.model

import scalafx.scene.paint.Color
import scalaExcel.formula._
import scalaExcel.CellPos

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

  /** Get the number of rows in the sheet */
  lazy val rows = if (cells.isEmpty) 0 else cells.keys.maxBy(_._2)._2 + 1
  /** Get the number of columns in the sheet */
  lazy val cols = if (cells.isEmpty) 0 else cells.keys.maxBy(_._1)._1 + 1
  /** Get a tuple of columns and rows in the sheet */
  lazy val size = (cols, rows)

  /**
   * Set the cell at pos to some formula f
   * @return the new sheet, and a list of cells that need to be recalculated
   */
  def setCell(pos : CellPos, f : String) : (Sheet, List[CellPos]) = {
    val newCell = Cell(f)
    setCell(pos, newCell)
  }

  private def setCell(pos: CellPos, newCell : Cell) = {
    val newCells = cells + (pos -> newCell)
    val newValues = calcNewValue(pos, newCell)
    val newDependents = calcNewDependents(pos, newCell)
    (new Sheet(newCells, newValues, newDependents, styles), dependentsOf(pos))
  }

  def deleteCell(p : CellPos) = {
    (new Sheet(cells - p, values - p, dependents - p, styles - p), dependentsOf(p))
  }

  /**
   * Copy a cell and change its dependencies relative to the new position
   */
  def copyCell(from: CellPos, to: CellPos) = {
    val cell = Cell(DependencyModifier.moveDependencies(from, to)(getCell(from).AST))
    (new Sheet(cells + (to -> cell), calcNewValue(to, cell), calcNewDependents(to, cell), styles), dependentsOf(to))
  }

  /**
   * Cut a cell to a new location and propagate the location change to its dependents.
   */
  def cutCell(from: CellPos, to: CellPos) = {
    val dependents = dependentsOf(from)
    val toUpdate = dependents ++ dependentsOf(to)

    val (tempSheet, _) = setCell(to, getCell(from))

    // Change all the dependent cells to point to the new cell
    val tempSheet2 = dependents.foldLeft(tempSheet){(s, pos) =>
      s.setCell(pos, Cell(DependencyModifier.changeDependency(from, to)(s.getCell(pos).AST)))._1
    }

    // Delete the original cell
    val (newSheet, _) = tempSheet2.deleteCell(from)

    (newSheet, toUpdate)
  }

  /**
   * recalculate the value of a cell
   * @return a new sheet which includes the new value, and a list of cells that also need to be updated
   */
  def updateCell(pos : CellPos) = {
    (new Sheet(cells, calcNewValue(pos, getCell(pos)), dependents, styles), dependentsOf(pos))
  }

  def getCellStyle(pos : CellPos) = styles.getOrElse(pos, Styles.DEFAULT)

  /** Set the style of a cell */
  def setCellStyle(pos : CellPos, s: Styles) = {
    new Sheet(cells, values, dependents, styles + (pos -> s))
  }

  /**
   * Set a cell to the circular reference error
   */
  def setToCircular(pos : CellPos) = {
    new Sheet(cells, values + (pos -> VErr(CircularRef)), dependents)
  }

  /** Get the cells that depend on this given cell */
  def dependentsOf(p: CellPos) : List[CellPos] = dependents getOrElse(p, List())

  def valueAt(pos : CellPos) = values get pos

  /** Get the Cell or return an empty cell */
  def getCell(pos : CellPos) : Cell = cells getOrElse(pos, Cell())

  def getValue(pos : CellPos) : Value = values getOrElse(pos, VEmpty)

  private def calcNewValue(pos: CellPos, c: Cell) = {
    val value = c.eval(values)
    values + (pos -> value)
  }

  private def calcNewDependents(pos: CellPos, c: Cell) = {
    val oldCell = getCell(pos)
    val oldDeps = oldCell.refs

    val r = c.refs
    val rmvDeps = oldDeps diff r
    val addDeps = r diff oldDeps

    val newDepsA = rmvDeps.foldLeft(dependents)((refsMap, ref) => refsMap get ref match {
      case Some(l) => refsMap + (ref -> (l diff List(pos)))
      case None => refsMap
    })

    addDeps.foldLeft(newDepsA)((refsMap, ref) => refsMap get ref match {
      case Some(l) => refsMap + (ref -> (l :+ pos))
      case None => refsMap + (ref -> List(pos))
    })
  }

  override def toString = (
    cells.toString(),
    dependents.toString(),
    values.toString()
    ).toString()

}
