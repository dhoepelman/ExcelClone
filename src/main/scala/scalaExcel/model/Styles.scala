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
    val align: Alignment
  )

object Styles {
  val DEFAULT = new Styles(Color.Black, Color.White, "", Left())
}
