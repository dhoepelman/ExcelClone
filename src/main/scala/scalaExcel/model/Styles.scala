package scalaExcel.model

import scalafx.scene.paint.Color

abstract class Alignment
case class Left()   extends Alignment
case class Center() extends Alignment
case class Right()  extends Alignment

class Styles(
    val background: Color,
    val color: Color,
    val format: String,
    val align: Alignment) {

  def setBackground(b: Color) = new Styles(b, color, format, align)
  def setColor(c: Color) = new Styles(background, c, format, align)
  def setFormat(f: String) = new Styles(background, color, f, align)
  def setAlign(a: Alignment) = new Styles(background, color, format, a)

  override def toString() =
    Map(
      "background" -> background,
      "text-fill" -> color
    ).toString;
}

object Styles {
  val DEFAULT = new Styles(Color.Azure, Color.Black, "", Left())
}
