
package scalaExcel

object AST {

  /*
  class Node[A] (v: A, l: List[Node[A]] = List()) {
    val leafs = l
    val value = v

    def map[B](f: (A => B)): Node[B] =
      new Node[B](
        f(value),
        leafs map (_ map f))

    def foreach(f: (A => Any)): Unit = {
      f(value)
      leafs foreach (_ foreach f)
    }

    def flatten: List[A] = {
      val head = List(value)
      val fleafs: List[List[A]] = leafs map (_ flatten)
      head ++ fleafs.flatten
    }
  }
  */

  abstract class CellVal
  case class CDouble(c: Double) extends CellVal
  case class CBool(c: Boolean) extends CellVal

  abstract class Node

  case class Num(d: Double) extends Node
  case class Bool(b: Boolean) extends Node

  abstract class Op
  case class Plus()   extends Op
  case class Min()    extends Op
  case class Mult()   extends Op
  case class Div()    extends Op
  case class Negate() extends Op

  case class BinOp(op: Op, l: Node, r: Node) extends Node
  case class UnOp(op: Op, n: Node) extends Node

  def eval (t: Node): Double = t match {
    case Num(d)          => d
    case BinOp(op, l, r) => evalBinOp(op, l, r)
    case UnOp(op, n)     => evalUnOp(op, n)
  }

  def evalBinOp (op: Op, l: Node, r: Node) = op match {
    case Plus() => (eval(l) + eval(r))
    case Min()  => (eval(l) - eval(r))
    case Mult() => (eval(l) * eval(r))
    case Div()  => (eval(l) / eval(r))
  }

  def evalUnOp (op: Op, n: Node) = op match {
    case Negate() => - eval(n)
  }

}

/*
abstract class Term {
  val leafs : List[Term] = List()
  def exec : Double
}

case class TermNum(d: Double) extends Term {
  def exec = d
}

case class TermBool(b: Boolean) extends Term {
  def exec = if (b) 1 else 0
}

// case class TermCell(c: Cell) extends Term {
//   def exec = c.getValue
// }

abstract class TermBinOp(l: Term, r: Term) extends Term {
  override val leafs = List(l, r)
}

case class TermAdd(l: Term, r: Term) extends TermBinOp(l, r) {
  def exec = l.exec + r.exec
}

case class TermMin(l: Term, r: Term) extends TermBinOp(l, r) {
  def exec = l.exec - r.exec
}

case class TermMult(l: Term, r: Term) extends TermBinOp(l, r) {
  def exec = l.exec * r.exec
}

abstract class TermUnOp(c: Term) extends Term {
  override val leafs = List(c)
}

case class TermAdd10(c: Term) extends TermUnOp(c) {
  def exec = c.exec + 10
}

case class TermInc(c: Term) extends TermUnOp(c) {
  def exec = c.exec + 1
}

case class TermDec(c: Term) extends TermUnOp(c) {
  def exec = c.exec - 1
}
*/
