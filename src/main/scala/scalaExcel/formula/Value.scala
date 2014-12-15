
package scalaExcel.formula;

sealed trait Value extends Ordered[Value]

case class VDouble(d: Double) extends Value {
  def compare(v: Value) = v match {
    case VDouble(d2) => d.compare(d2)
    case _ => -1
  }
}

case class VString(s: String) extends Value {
  def compare(v: Value) = v match {
    case VString(s2) => s.compare(s2)
    case VDouble(_) => 1
    case _ => -1
  }
}

case class VBool(b: Boolean) extends Value {
  def compare(v: Value) = v match {
    case VBool(b2) => b.compare(b2)
    case VString(_) | VDouble(_) => 1
    case _ => -1
  }
}

case class VErr(t: ErrType) extends Value {
  def compare(v: Value) = v match {
    case VEmpty => -1
    case _ => 1
  }
}

case object VEmpty extends Value {
  def compare(v : Value) = v match {
    case VEmpty => 0
    case _ => 1
  }
}

object Values {
  def toVal(v: Any) = v match {
    case d: Double  => VDouble(d)
    case i: Int     => VDouble(i.toDouble)
    case s: String  => VString(s)
    case b: Boolean => VBool(b)
    case _          => VErr(NA)
  }
}
