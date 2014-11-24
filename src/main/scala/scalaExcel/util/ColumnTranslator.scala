package scalaExcel.util

object ColumnTranslator {
  // A => 0, B => 1, AA => 26
  def colToNum(r: String): Int = r.foldLeft(-1)(_ * 26 + _ - 39)

  // 0 => A, 1 => B, 26 => AA
  def numToCol(r: Int): String =
    if (r >= 1) numToCol(r / 26 - 1) + (r % 26 + 65).toChar
    else if (r == 0) "A"
    else ""
}