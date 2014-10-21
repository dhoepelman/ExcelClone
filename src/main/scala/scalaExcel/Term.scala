
package scalaExcel

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

