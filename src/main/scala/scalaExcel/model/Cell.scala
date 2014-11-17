package scalaExcel.model

import scalaExcel.formula.{VDouble, Value}

// This is a cell object, it can execute it self and find the references inside
// the formula. This implements a dummy parser/executor
class Cell(val x: Int, val y: Int, val f: String) {

  /*
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
   */

  def refs : List[(Int,Int)] = ???

  val position = (x, y)

   // Add all numbers and references
   def exec(deps: Map[(Int, Int), Value]) : Value = ???
   /*refs.foldLeft(lhs + rhs)((sum, ref) => deps get ref match {
     case Some(x) => x match {
       case VDouble(d) => VDouble(sum + d)
       case _ => ???
     }
     case None => throw new Exception(s"I, ($x,$y), am trying to use a non existing $ref")
   })*/

 }
