package scalaExcel.formula

object ReferenceFinder {

  sealed trait Node
  case class Leaf(e: Expr) extends Node
  case class Branch(l: List[Expr]) extends Node

  def find(e: Expr): List[Expr] = find(e, List[Expr]())

  def find(e: Expr, l: List[Expr]): List[Expr] = getChildren(e) match {
    case Leaf(e)   => l.+:(e)
    case Branch(l) => l map (x => find(x)) flatten
  }

  // get all child nodes
  def getChildren(e: Expr) = e match {
    case BinOp(_, l, r)       => Branch(List(l, r))
    case UnOp(_, e)           => Branch(List(e))
    case Call(_, args)        => Branch(args)
    case SheetReference(_, e) => Branch(List(e))
    case e                    => Leaf(e)
  }

}
