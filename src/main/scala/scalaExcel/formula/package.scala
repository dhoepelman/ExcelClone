package scalaExcel

package object formula {
  /**
   * Converts a column string to corresponding number
   * @example A => 1, B => 2, AA => 27
   */
  def numToCol(r: Int): String =
    if (r >= 1) numToCol((r - 1) / 26) + ((r - 1) % 26 + 65).toChar
    else ""

  /**
   * Converts a column number to corresponding string
   */
  def colToNum(r: String): Int = r.foldLeft(0)(_ * 26 + _ - 64)
}
