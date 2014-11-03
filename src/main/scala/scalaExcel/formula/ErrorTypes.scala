
package scalaExcel.formula;

sealed abstract class ErrType

case class DivBy0() extends ErrType
case class NA() extends ErrType
case class InvalidName() extends ErrType
case class NotNumeric() extends ErrType
case class Null() extends ErrType
case class InvalidRef() extends ErrType
case class InvalidValue() extends ErrType

