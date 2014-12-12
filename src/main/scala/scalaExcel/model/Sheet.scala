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
  def setCell(pos: CellPos, f: String): Sheet = {
    val newCell = Cell(f)
    setCell(pos, newCell)
  }

  private def setCell(pos: CellPos, newCell: Cell): Sheet = {
    val newCells = cells + (pos -> newCell)
    val newValues = calcNewValue(pos, newCell)
    val newDependents = calcNewDependents(pos, newCell)
    Sheet.updateSheet(
      new Sheet(newCells, newValues, newDependents, styles),
      dependentsOf(pos),
      Set(pos))
  }

  /** Get the Cell or return an empty cell */
  def getCell(pos: CellPos) : Cell = cells getOrElse(pos, Cell())

  def valueAt(pos: CellPos) = values get pos

  def getValue(pos: CellPos) : Value = valueAt(pos).getOrElse(VEmpty)

  def deleteCell(p: CellPos): Sheet = {
    Sheet.updateSheet(
      new Sheet(cells - p, values - p, dependents - p, styles - p),
      dependentsOf(p))
  }

  def deleteCells(poss: Traversable[CellPos]): Sheet = {
    poss.foldLeft(this)((sheet, pos) => sheet.deleteCell(pos))
  }

  /**
   * Copy a cell and change its dependencies relative to the new position
   */
  def copyCell(from: CellPos, to: CellPos) = {
    val cell = Cell(DependencyModifier.moveDependencies(from, to)(getCell(from).AST))
    Sheet.updateSheet(
      new Sheet(
        cells + (to -> cell),
        calcNewValue(to, cell),
        calcNewDependents(to, cell),
        styles + (to -> getCellStyle(from))),
      dependentsOf(to))
  }

  /**
   * Cut a cell to a new location and propagate the location change to its dependents.
   */
  def cutCell(from: CellPos, to: CellPos) = {
    val dependents = dependentsOf(from)
    val toUpdate = dependents ++ dependentsOf(to)

    val tempSheet = this
      .setCell(to, getCell(from))
      .setCellStyle(to, getCellStyle(from))

    // Change all the dependent cells to point to the new cell
    val tempSheet2 = dependents.foldLeft(tempSheet){(s, pos) =>
      s.setCell(pos, Cell(DependencyModifier.changeDependency(from, to)(s.getCell(pos).AST)))
    }

    // Delete the original cell
    val newSheet = tempSheet2.deleteCell(from)

    Sheet.updateSheet(newSheet, toUpdate)
  }

  //TODO implement
  /**
   * Invalidate all AST references to the given columns
   * @param columns list of column indexes
   * @return        sheet containing modified cells
   */
  def removeReferencesToColumns(columns: List[Int]) = this

  //TODO implement
  /**
   * Invalidate all AST references to the given rows
   * @param rows list of row indexes
   * @return        sheet containing modified cells
   */
  def removeReferencesToRows(rows: List[Int]) = this

  /** Set the style of a cell */
  def setCellStyle(pos : CellPos, s: Styles) = {
    new Sheet(cells, values, dependents, styles + (pos -> s))
  }

  def updateCellStyle(pos: CellPos, f: Styles => Styles) = {
    setCellStyle(pos, f(getCellStyle(pos)))
  }

  def updateCellsStyle(poss: Traversable[CellPos], f: Styles => Styles) = {
    poss.foldLeft(this)((sheet, pos) => sheet.updateCellStyle(pos, f))
  }

  def getCellStyle(pos : CellPos) = styles.getOrElse(pos, Styles.DEFAULT)

  /**
   * recalculate the value of a cell
   * @return a new sheet which includes the new value, and a list of cells that
   *         also need to be updated
   */
  private def updateCell(pos : CellPos) = {
    (new Sheet(cells, calcNewValue(pos, getCell(pos)), dependents, styles), dependentsOf(pos))
  }

  /**
   * Set a cell to the circular reference error
   */
  private def setToCircular(pos : CellPos) = {
    new Sheet(cells, values + (pos -> VErr(CircularRef)), dependents)
  }

  /** Get the cells that depend on this given cell */
  private def dependentsOf(p: CellPos) : List[CellPos] = dependents getOrElse(p, List())

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

object Sheet {

  /**
   * function to propagate updates to dependent cells
   * @param alreadyUpdated Set of cells that were already updated, to detect cycles
   */
  def updateSheet(s: Sheet, updates: List[CellPos], alreadyUpdated: Set[CellPos] = Set()): Sheet = {
    updates.foldLeft(s)((s, u) => {
      if (alreadyUpdated contains u)
        // u was already updated, so this means there's a circular reference
        s.setToCircular(u)
      else
        s.updateCell(u) match {
          case (newSheet, List()) => newSheet
          case (newSheet, newUpdates) => updateSheet(newSheet, newUpdates, alreadyUpdated + u)
        }
    })
  }

  def updateSheet(x: (Sheet, List[(Int,Int)])): Sheet = x match {
    case (s, updates) => updateSheet(s, updates)
  }

}
