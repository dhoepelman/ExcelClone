
abstract class Term
abstract case class BinOp(op: String, l: Term, r: Term) extends Term
case class Plus(l: Term, r: Term) extends Term {
  val op = "+"
}

case class IntLit(v: String) extends Term

object Hi {
  def main(args: Array[String]) {

    val t: Term = Plus(IntLit("4"), IntLit("5"))


    val x = t match { case Plus(IntLit(l), IntLit(r)) => {
      "Adding " + l + " and " + r
    }}

    println(x)

  }
}
