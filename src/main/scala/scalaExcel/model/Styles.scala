package scalaExcel.model

import java.text.{DecimalFormatSymbols, DecimalFormat, NumberFormat}
import java.util.Locale

import scalaExcel.formula._
import scalaExcel.util.DefaultProperties
import scalafx.scene.paint.Color
import scalaExcel.formula.Evaluator.boolToString

abstract class Alignment
case object LeftAlign   extends Alignment
case object CenterAlign extends Alignment
case object RightAlign  extends Alignment
case object NoAlign     extends Alignment

abstract class ValueFormat {
  def apply(value: Value): String
}
case object DefaultValueFormat extends ValueFormat {
  def apply(value: Value): String = value match {
    case VDouble(d) => new CustomNumericValueFormat().apply(value)
    case _ => TextValueFormat.apply(value)
  }
  override def toString = "Default"
}
case object TextValueFormat extends ValueFormat {
  def apply(value: Value): String = value match {
    case VEmpty => ""
    case VDouble(d) => d.toString
    case VString(s) => s
    case VBool(b) => boolToString(b)
    case VErr(e) => e.expr
    case _ => value.toString
  }
  override def toString = "Text"
}
case object ScientificValueFormat extends ValueFormat {
  def apply(value: Value): String = value match {
    case VDouble(d) => ""
    case _ => DefaultValueFormat.apply(value)
  }
  override def toString = "Scientific"
}
case object PercentageValueFormat extends ValueFormat {
  def apply(value: Value): String = value match {
    case VDouble(d) => ""
    case _ => DefaultValueFormat.apply(value)
  }
  override def toString = "Percentage"
}
case object CurrencyValueFormat extends ValueFormat {
  def apply(value: Value): String = value match {
    case VDouble(d) =>
      val locale = if (Locale.getDefault == null) Locale.US else Locale.getDefault
      val formatter = NumberFormat.getCurrencyInstance(locale)
      formatter.format(d)
    case _ => DefaultValueFormat.apply(value)
  }
  override def toString = "Currency"
}
case class CustomNumericValueFormat(
                          prefix: String = "",
                          suffix: String = "",
                          decimalPlaces: Int = DefaultProperties.NF_DECIMAL_PLACES,
                          enableGrouping: Boolean = DefaultProperties.NF_ENABLE_GROUPING,
                          decimalSymbol: Option[Char] = None,
                          groupingSymbol: Option[Char] = None
                          ) extends ValueFormat {

  def apply(value: Value): String = value match {
    case VDouble(d) =>
      val locale = if (Locale.getDefault == null) Locale.US else Locale.getDefault

      val formatter = new DecimalFormat()
      formatter.setGroupingUsed(enableGrouping)
      formatter.setMaximumFractionDigits(decimalPlaces)

      val symbols = new DecimalFormatSymbols(locale)
      symbols.setDecimalSeparator(decimalSymbol.getOrElse(symbols.getDecimalSeparator))
      symbols.setGroupingSeparator(groupingSymbol.getOrElse(symbols.getGroupingSeparator))
      formatter.setDecimalFormatSymbols(symbols)

      prefix + formatter.format(d) + suffix
    case _ => DefaultValueFormat.apply(value)
  }
  override def toString = "Custom"
}

class Styles (
    val background: Color,
    val color: Color,
    val format: ValueFormat,
    val align: Alignment) {

  def setBackground(b: Color) = new Styles(b, color, format, align)
  def setColor(c: Color) = new Styles(background, c, format, align)
  def setFormat(f: ValueFormat) = new Styles(background, color, f, align)
  def setAlign(a: Alignment) = new Styles(background, color, format, a)

  override def toString =
    Map(
      "background" -> background,
      "text-fill" -> color
    ).toString()

  override def equals(other: Any) = other match {
    case that: Styles =>
      background == that.background &&
      color == that.color &&
      format == that.format &&
      align == that.align
    case _ => false
  }


}

object Styles {
  val DEFAULT = new Styles(Color.WhiteSmoke, Color.Black, DefaultValueFormat, NoAlign)
}
