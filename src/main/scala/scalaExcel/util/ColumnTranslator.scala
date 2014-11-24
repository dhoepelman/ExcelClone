package scalaExcel.util

object ColumnTranslator {

  // A => 1, B => 2, AA => 27
  def colToNum(r: String): Int = r.foldLeft(0)(_ * 26 + _ - 64)

  // 1 => A, B => 2, 27 => AA
  def numToCol(r: Int): String =
    if (r >= 1) numToCol((r - 1) / 26) + ((r - 1) % 26 + 65).toChar
    else ""

}
