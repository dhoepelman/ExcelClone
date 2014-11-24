
package scalaExcel.formula

sealed trait ErrType {
  /** What users see when this error is encountered */
  def expr : String
}

/**
 * #DIV/0! Trying to divide by 0
 **/
case class DivBy0() extends ErrType {
  val expr = "#DIV/0!"
}
/**
 * #N/A! A formula or a function inside a formula cannot find the referenced data
 **/
case class NA() extends ErrType {
  val expr = "#N/A!"
}
/**
 * #NAME? Text in the formula is not recognized
 **/
case class InvalidName() extends ErrType {
  val expr = "#NAME?"
}
/**
 * #NUM! A formula has invalid numeric data for the type of operation
 **/
case class NotNumeric() extends ErrType {
  val expr = "#NUM!"
}
/**
 * #NULL! A space was used in formulas that reference multiple ranges; a comma separates range references
 **/
case class Null() extends ErrType {
  val expr = "#NULL!"
}
/**
 * #REF! A reference is invalid
 **/
case class InvalidRef() extends ErrType {
  val expr = "#REF!"
}
/**
 * #VALUE! The wrong type of operand or function argument is used
 **/
case class InvalidValue() extends ErrType {
  val expr = "#VALUE!"
}
/**
 * #CIRCULAR! Custom error type for circular references
 */
case class CircularRef() extends ErrType {
  val expr = "#CIRCULAR!"
}

