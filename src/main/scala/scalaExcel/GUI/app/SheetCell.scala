package scalaExcel.GUI.app

import scalafx.beans.property.{StringProperty, ObjectProperty}

class SheetCell(objectValue_ : Any,
                formatter_ : Any => String,
                stylist_ : Any => String) {
  val objectValue = new ObjectProperty(this, "objectValue", objectValue_)
  val formatter =
    if (formatter_ == null)
      new ObjectProperty(this, "formatter", (v: Any) => v.toString)
    else
      new ObjectProperty(this, "formatter", formatter_)
  val stylist = new ObjectProperty(this, "stylist", (_: Any) => "")
  val objectString = new StringProperty(this, "objectString", formatter.value(objectValue.value))
  val asProperty = new ObjectProperty(this, "asProperty", this)

  override def toString: String = objectString.value
}
