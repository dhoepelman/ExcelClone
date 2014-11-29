package scalaExcel.util

object ColumnTranslator {
  /**
   * Converts a column string to corresponding number
   * @example A => 0, B => 1, AA => 26
   */
  def colToNum(r: String): Int = r.foldLeft(-1)(_ * 26 + _ - 39)

  /**
   * Converts a column number to corresponding string
   */
  def numToCol(r: Int): String =
    if (r >= 1) numToCol(r / 26 - 1) + (r % 26 + 65).toChar
    else if (r == 0) "A"
    else ""
}