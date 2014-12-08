package scalaExcel.GUI.util

import scalaExcel.model.{Styles, RightAlign, CenterAlign, LeftAlign, NoAlign}
import scalafx.scene.paint.Color

object CSSHelper {

  private def colorToWeb(c: Color): String =
    "#%02X%02X%02X".format(
      (c.red * 255).asInstanceOf[Int],
      (c.green * 255).asInstanceOf[Int],
      (c.blue * 255).asInstanceOf[Int])

  private def fieldsFromCss(css: String): (Map[String, String]) = {
    val bodyRe = """([^:;{}]+:[^:;{}]+;?)""".r
    bodyRe.findAllIn(css)
      .map(pair => pair.split(":"))
      .map(tokens => tokens(0).trim -> tokens(1).trim.replace(";", ""))
      .foldLeft(Map[String, String]())((m, x) => m + x)
  }

  private def fieldsToCss(fields: Map[String, String]): String =
    fields.foldLeft("")((s, x) => s + x._1 + ": " + x._2 + "; ")


  private def setCssField(css: String, field: String, value: String): String =
    fieldsToCss(fieldsFromCss(css) + (field -> value))


  private def setCssField(css: String, property: String): String = {
    val tokens = property.split(":").map(x => x.trim)
    setCssField(css, tokens(0), tokens(1))
  }

  val insetsProperty = "-fx-background-insets"
  val insetsValue = "0, 0 0 1 0"

  val errorStyle = "-fx-background-color: -fx-table-cell-border-color, #FF0000; " +
    insetsProperty + ":" + insetsValue + ";"

  def modifyStyleProperty(oldStyle: String, property: String, value: Any) = {
    val stringVal = value match {
      case v: String => v
      case v: Color => colorToWeb(v)
    }
    property match {
      case "-fx-background-color" =>
        fieldsToCss(fieldsFromCss(oldStyle) +
          (property -> ("-fx-table-cell-border-color, " + stringVal)) +
          (insetsProperty -> insetsValue))
      case _ => setCssField(oldStyle, property, stringVal)
    }

  }

  def propertyFromCssOrElse(css: String, property: String, orElse: String): String = property match {
    case "-fx-background-color" => fieldsFromCss(css).getOrElse(property, orElse).split(",").last.trim()
    case _ => fieldsFromCss(css).getOrElse(property, orElse)
  }

  def colorFromCssOrElse(css: String, property: String, orElse: Color): Color =
    Color.web(propertyFromCssOrElse(css, property, colorToWeb(orElse)))


  def getCSSFromStyle(style: Styles): String = {
    fieldsToCss(toCSSMap(style))
  }

  def toCSSMap(style: Styles): Map[String, String] = {
    Map(
      "-fx-background-color" -> ("-fx-table-cell-border-color, " + colorToWeb(style.background)),
      "-fx-text-fill" -> colorToWeb(style.color),
      "-fx-background-insets" -> "0, 0 0 1 0",
      "-fx-alignment" -> (style.align match {
        case LeftAlign => "CENTER-LEFT"
        case RightAlign => "CENTER-RIGHT"
        case CenterAlign => "CENTER"
        case NoAlign => "CENTER-LEFT"
      })
    )
  }

  def asError(style : Styles) : String = {
    fieldsToCss(toCSSMap(style)
    + ("-fx-background-color" -> "-fx-table-cell-border-color, red")
    // Meager attempts to make a small red triangle in the cell
    //+ ("-fx-shape" -> "\"M 0 0 v -10 l 5 5 z\"")
    + ("-fx-text-fill" -> colorToWeb(Color.White))
    )
  }
}
