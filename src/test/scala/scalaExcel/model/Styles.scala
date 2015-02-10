package scalaExcel.model

import java.util.Locale
import scalaExcel.formula._

import org.junit.Assert._
import org.junit._

class StylesTests {

  def testOthers(style: Styles) {
    assertEquals("hello", style.format(VString("hello")))
    assertEquals("TRUE", style.format(VBool(true)))
    assertEquals("FALSE", style.format(VBool(false)))
    assertEquals("", style.format(VEmpty))
  }

  @Test def testDefaultFormat() {
    val style = Styles.DEFAULT.setFormat(DefaultValueFormat)
    assertEquals("123.46", style.format(VDouble(123.4567)))
    testOthers(style)
  }

  @Test def testTextFormat() {
    val style = Styles.DEFAULT.setFormat(TextValueFormat)
    assertEquals("123.4567", style.format(VDouble(123.4567)))
    testOthers(style)
  }

  @Test def testScientificFormat() {
    val style = Styles.DEFAULT.setFormat(ScientificValueFormat)
    assertEquals("1.23E02", style.format(VDouble(123.4567)))
    assertEquals("1.23E00", style.format(VDouble(1.23)))
    assertEquals("1.23E-04", style.format(VDouble(0.000123)))
    testOthers(style)
  }

  @Test def testPercentageFormat() {
    val style = Styles.DEFAULT.setFormat(PercentageValueFormat)
    assertEquals("0%", style.format(VDouble(0)))
    assertEquals("100%", style.format(VDouble(1)))
    assertEquals("50%", style.format(VDouble(0.5)))
    testOthers(style)
  }

  @Test def testCurrencyFormat() {
    Locale.setDefault(Locale.US)
    val style = Styles.DEFAULT.setFormat(CurrencyValueFormat)
    assertEquals("$0.00", style.format(VDouble(0)))
    assertEquals("$10.00", style.format(VDouble(10)))
    assertEquals("$1.23", style.format(VDouble(1.234)))
    testOthers(style)
  }

  @Test def testCustomNumericValueFormat() {
    val style = Styles.DEFAULT.setFormat(CustomNumericValueFormat(
      "--", "__", 2, 4, 3, 5, Some('*'), true, Some('@')))
    assertEquals("--01*000__", style.format(VDouble(1)))
    assertEquals("--4@567*000__", style.format(VDouble(1234567)))
    assertEquals("--01*23457__", style.format(VDouble(1.234567)))
    testOthers(style)
  }

}
