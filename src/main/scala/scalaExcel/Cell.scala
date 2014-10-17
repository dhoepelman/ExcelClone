
package scalaExcel

case class Cell(f1: Term) {

  // cached value, if it's there
  var value: Option[Int] = None
  // cell's formula / function
  var f: Option[Term] = None
  // these cells depend on this cell
  var dependents:  List[Cell] = List()
  var dependencies: List[Cell] = List()

  def addDependent(d: Cell) = dependents = dependents :+ d

  def removeDependent(d: Cell) = dependents = dependents diff List(d)

  def findCells(xs: List[Term]): List[Cell] = {
    xs match {
      case TermCell(cell) :: tail => List(cell) ++ findCells(tail)
      case (head: Term) :: tail => findCells(head.leafs) ++ findCells(tail)
      case List() => List()
    }
  }

  def findDepCells = f match {
    case Some(t) => findCells(List(t))
    case _ => List()
  }

  def setFunc(f2: Term) = {
    // new function, remove parent of the previous children
    dependencies foreach (_.removeDependent(this))
    // set new function
    f = Some(f2)
    // set the parent/dependencies of the new function
    dependencies = findDepCells
    dependencies foreach (_.addDependent(this))
    // reset values
    dependents :+ this foreach (_.clearValue)
  }

  // execute the function
  def exec: Int = {

    val x = f match {
      case Some(_f) => _f.exec
      case None => 0
    }

    value = Some(x)

    return x
  }

  // get the value, if cached, otherwise execute and return
  def getValue: Int = value match {
    case Some(x) => x
    case None => exec
  }

  // when another cell this cell depends on,
  // this method can clear the cached value
  def clearValue = value = None

  setFunc(f1)

}
