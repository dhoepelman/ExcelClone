package scalaExcel

import rx.lang.scala.{Subject, Observable, Observer}

// Types of inputs to the data model
abstract class SheetMutations
case class SetCell(x: Int, y: Int, f: String) extends SheetMutations

// This is a cell object, it can execute it self and find the references inside
// the formula. This implements a dummy parser/executor
class Cell(val x: Int, val y: Int, val f: String) extends Observer[Int] {

  val sumInt = """=(\d+)\+(\d+)""".r
  val sumRef = """=([A-Z]+)\+([A-Z]+)""".r

  // either the formula is adding two numbers
  val (lhs, rhs) = sumInt findFirstIn f match {
    case Some(sumInt(lhs, rhs)) => (lhs.toInt, rhs.toInt)
    case None => (0, 0)
  }

  // or it has two references that are added
  val refs = ((sumRef findFirstIn f) match {
    case Some(sumRef(lhs, rhs)) => List(lhs, rhs)
    case _ => List()
  }) map (_ match {
    case "A" => (1, 1)
    case "B" => (2, 1)
    case x => throw new Exception(s"Don't know $x?!")
  })

  val p = (x, y)

  // Add all numbers and references
  def exec(deps: Map[(Int, Int), Int]) = refs.foldLeft(lhs + rhs)((sum, ref) => deps get ref match {
    case Some(x) => sum + x
    case None => throw new Exception(s"I, ($x,$y), am trying to use a non existing $ref")
  })

}

// A Sheet is currently the entire immutable datamodel, containing:
// - the cells, objects of the cell Class, that know how to execute themselves
// - final values of all positions in the sheet
// - a map lists of cells that depend on that cell
class Sheet(
    val cells: Map[(Int, Int), Cell] = Map(),
    val values: Map[(Int, Int), Int] = Map(),
    val refs: Map[(Int, Int), List[(Int, Int)]] = Map()) {

  // Set the cell at (x,y) to some formula f, return the new sheet, and a list
  // of cells that need to be recalculated
  def setCell(x: Int, y: Int, f: String) = {
    val newCell   = new Cell(x, y, f)
    val newCells  = cells + (newCell.p -> newCell)
    val newValues = calcNewValue(newCell)
    val newRefs   = calcNewRefs(newCell)
    (new Sheet(newCells, newValues, newRefs), dependentsOf(newCell))
  }

  // recalculate the value of a cell, return a new sheet which includes the new
  // value, and a list of cells that also need to be updated
  def updateCell(x: Int, y: Int) = {
    cells get ((x,y)) match {
      case Some(c) => (new Sheet(cells, calcNewValue(c), refs), dependentsOf(c))
      case None => (this, List[(Int,Int)]())
    }
  }

  // Get the cells that depend on this given cell
  def dependentsOf(c: Cell) = refs get c.p match {
    case Some(l) => l
    case None => List()
  }

  def valueAt(x: Int, y: Int) = values get ((x, y))

  private def calcNewValue(c: Cell) = {
    var dependencies = getDependencyValues(c.refs)
    val value = c.exec(dependencies)
    values + (c.p -> value)
  }

  private def getDependencyValues(deps: List[(Int,Int)]) = {
    deps.foldLeft(Map[(Int, Int), Int]())((map, ref) => values get ref match {
      case Some(x) => map + (ref -> x)
      case None => throw new Exception(s"Couldn't find the value of $ref")
    })
  }

  private def calcNewRefs(c: Cell) = {
    val oldCell = cells get(c.p)
    val oldRefs = oldCell match {
      case Some(c) => c.refs
      case None => List()
    }

    val r = c.refs
    val rmvRefs = oldRefs diff r
    val addRefs = r diff oldRefs

    val newRefsA = rmvRefs.foldLeft(refs)((refsMap, ref) => refsMap get ref match {
      case Some(l) => refsMap + (ref -> (l diff List(c.p)))
      case None => refsMap
    })

    addRefs.foldLeft(newRefsA)((refsMap, ref) => refsMap get ref match {
      case Some(l) => refsMap + (ref -> (l :+ c.p))
      case None => refsMap + (ref -> List(c.p))
    })
  }

  override def toString = (
    cells.toString(),
    refs.toString(),
    values.toString()
  ).toString()

}

object Rx extends App {

  // This is a stream of inputs from 'the world' that will effect the state of
  // the sheet model
  val sheetMutations = Subject[SheetMutations]()

  // function to propagate updates to dependent cells
  def updateSheet(s: Sheet, updates: List[(Int, Int)]): Sheet = {
    updates.foldLeft(s)((s, u) => s.updateCell(u._1, u._2) match {
      case (newSheet, List()) => newSheet
      case (newSheet, newUpdates) => updateSheet(newSheet, newUpdates)
    })
  }

  def updateSheet(x: (Sheet, List[(Int,Int)])): Sheet = x match {
    case (s, updates) => updateSheet(s, updates)
  }

  // this combines the initial Sheet with all input mutations from the outside
  // world
  val sheet = sheetMutations.scan(new Sheet())((sheet, action) => action match {
    case SetCell(x, y, f) => updateSheet(sheet.setCell(x, y, f))
  })

  // Anything can subscribe to this stream of
  sheet.subscribe(x => println(x.values))

  // Or to just get the distinct values at cell (3, 1)
  sheet
    .map(_.valueAt(3, 1))
    .filter(!_.isEmpty)
    .map(_.get)
    .distinctUntilChanged
    .subscribe(x => println(s"value at (3,1) changed to $x"))

  // Or we can define Observable[Sheet] method extensions
  implicit class ExtendedObservableSheet(val sheet: Observable[Sheet]) extends AnyVal {
    def filterCellValueAt(x: Int, y: Int) =
      sheet
        .map(_.valueAt(x, y))
        .filter(!_.isEmpty)
        .map(_.get)
        .distinctUntilChanged
  }

  // And use the this:
  sheet.filterCellValueAt(4, 1).subscribe(x => println(s"(4,1) value changed to $x"))

  // Input some changes
  sheetMutations.onNext(SetCell(1, 1, "=1+2"))
  sheetMutations.onNext(SetCell(2, 1, "=A+A"))
  sheetMutations.onNext(SetCell(3, 1, "=A+B"))
  sheetMutations.onNext(SetCell(4, 1, "=A+A"))
  sheetMutations.onNext(SetCell(1, 1, "=4+6"))
  sheetMutations.onNext(SetCell(2, 1, "=4+0"))

}
