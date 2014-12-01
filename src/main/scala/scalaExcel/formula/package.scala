package scalaExcel

package object formula {
  /**
   * Converts a column string to corresponding number
   * @example A => 0, B => 1, AA => 26
   */
  def colToNum(r: String): Int = {
    r.toUpperCase.foldLeft(-1)({ (acc, c) =>
      if(c < 'A' || c > 'Z')
        throw new IllegalArgumentException("Invalid column character '" + c + "'")
      else
        acc * 26 + c - 39}
    )
  }

  /**
   * Converts a column number to corresponding string
   */
  def numToCol(r: Int): String =
    if (r >= 1) numToCol(r / 26 - 1) + (r % 26 + 65).toChar
    else if (r == 0) "A"
    else ""
}
