
package scalaExcel.formula;

sealed abstract class ErrType

/**
 * #DIV/0! Trying to divide by 0
 **/
case class DivBy0() extends ErrType
/**
 * #N/A! A formula or a function inside a formula cannot find the referenced data
 **/
case class NA() extends ErrType
/**
 * #NAME? Text in the formula is not recognized
 **/
case class InvalidName() extends ErrType
/**
 * #NUM! A formula has invalid numeric data for the type of operation
 **/
case class NotNumeric() extends ErrType
/**
 * #NULL! A space was used in formulas that reference multiple ranges; a comma separates range references
 **/
case class Null() extends ErrType
/**
 * #REF! A reference is invalid
 **/
case class InvalidRef() extends ErrType
/**
 * #VALUE! The wrong type of operand or function argument is used
 **/
case class InvalidValue() extends ErrType

