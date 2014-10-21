package scalaExcel

import scala.util.parsing.combinator._

class Parser2 extends RegexParsers {
  def boolean: Parser[Expression] = ("true" | "false") ^^ {s => new BooleanLiteral(s.toBoolean)}
  def number : Parser[Expression] = """\d+(\.\d+)?""".r ^^ { s => new NumberLiteral(s.toDouble)}

  def factor : Parser[Expression] = number //| "(" ~ expr ~ ")"
}

sealed trait Expression {
  def eval(env:Map[String,Literal]) : Literal
}

sealed trait Literal extends Expression {
  def eval(env:Map[String,Literal]) = this
  def doubleValue : Double
  def boolValue : Boolean
  def stringValue : String
}
case class NumberLiteral(literal : Double) extends Literal {
  override def doubleValue: Double = literal
  override def stringValue: String = literal.toString
  override def boolValue: Boolean = literal != 0.0
  override def toString = literal.toString
}
case class BooleanLiteral(literal : Boolean) extends Literal {
  override def doubleValue: Double = if(literal) 1.0 else 0.0
  override def stringValue: String = literal.toString
  override def boolValue: Boolean = literal
  override def toString = literal.toString
}
/*
case class StringLiteral(s : String) extends Literal {
  val literal = s.substring(1,s.length-1)
  override def doubleValue: Double = literal.toDouble
  override def stringValue: String = literal
  override def boolValue: Boolean = literal.toLowerCase == "false"
  override def toString = literal.toString
}*/

case class Variable(name:String) extends Expression {
  override def eval(env: Map[String, Literal]): Literal = env(name)
  override def toString = name
}