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
  def applyNumericFormatter(formatter: (Locale) => DecimalFormat)(value: Value) =
    value match {
      case VDouble(d) =>
        val locale = if (Locale.getDefault == null) DefaultProperties.LOCALE else Locale.getDefault
        formatter(locale).format(d)
      case _ => DefaultValueFormat.apply(value)
    }
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
  def apply(value: Value): String =
    applyNumericFormatter((locale: Locale) =>
      new DecimalFormat("0.##E00", new DecimalFormatSymbols(locale)))(value)

  override def toString = "Scientific"
}
case object PercentageValueFormat extends ValueFormat {
  def apply(value: Value): String = value match {
    case VDouble(d) => new CustomNumericValueFormat(suffix="%").apply(VDouble(d*100))
    case _ => DefaultValueFormat.apply(value)
  }

  override def toString = "Percentage"
}
case object CurrencyValueFormat extends ValueFormat {
  def apply(value: Value): String =
    applyNumericFormatter({
      locale => NumberFormat.getCurrencyInstance(locale)
        .asInstanceOf[DecimalFormat]
    })(value)

  override def toString = "Currency"
}
case class CustomNumericValueFormat(
                          prefix: String = "",
                          suffix: String = "",
                          minIntegerDigits: Int = DefaultProperties.NF_MIN_INTEGER_DIGITS,
                          maxIntegerDigits: Int = DefaultProperties.NF_MAX_INTEGER_DIGITS,
                          minFractionDigits: Int = DefaultProperties.NF_MIN_FRACTION_DIGITS,
                          maxFractionDigits: Int = DefaultProperties.NF_MAX_FRACTION_DIGITS,
                          decimalSymbol: Option[Char] = None,
                          enableGrouping: Boolean = DefaultProperties.NF_ENABLE_GROUPING,
                          groupingSymbol: Option[Char] = None
                          ) extends ValueFormat {

  private def customFormatter(locale: Locale) = {
    val formatter = new DecimalFormat()
    formatter.setGroupingUsed(enableGrouping)
    formatter.setMinimumFractionDigits(minFractionDigits)
    formatter.setMaximumFractionDigits(maxFractionDigits)
    formatter.setMinimumIntegerDigits(minIntegerDigits)
    formatter.setMaximumIntegerDigits(maxIntegerDigits)

    val symbols = new DecimalFormatSymbols(locale)
    symbols.setDecimalSeparator(decimalSymbol.getOrElse(symbols.getDecimalSeparator))
    symbols.setGroupingSeparator(groupingSymbol.getOrElse(symbols.getGroupingSeparator))
    formatter.setDecimalFormatSymbols(symbols)
    formatter
  }
  def apply(value: Value): String =
    prefix + applyNumericFormatter(customFormatter)(value) + suffix

  override def toString = "Custom"

  override def equals(obj: scala.Any): Boolean = obj match {
    case _: CustomNumericValueFormat => true
    case _ => false
  }
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
