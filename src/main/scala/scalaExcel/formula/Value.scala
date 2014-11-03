
package scalaExcel.formula;

sealed abstract class Value

case class VDouble(d: Double) extends Value
case class VString(s: String) extends Value
case class VBool(b: Boolean) extends Value
case class VErr(t: ErrType) extends Value

object Values {
  def toVal(v: Any) = v match {
    case d: Double  => VDouble(d)
    case i: Int     => VDouble(i.toDouble)
    case s: String  => VString(s)
    case b: Boolean => VBool(b)
    case _          => VErr(NA())
  }
}
