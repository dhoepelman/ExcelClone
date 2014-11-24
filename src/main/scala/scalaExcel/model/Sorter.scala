package scalaExcel.model

import scalaExcel.formula._

object Sorter {

  def compare(a: Value, b: Value) = (a, b) match {
    case (VDouble(d1), VDouble(d2)) => d1 < d2
    case (VDouble(d), VString(s)) => true
    case (VDouble(d), VBool(b)) => true
    case (VDouble(d), VErr(e)) => true
    case (VString(s1), VString(s2)) => s1.compareTo(s2) < 0
    case (VString(s), VDouble(d)) => false
    case (VString(s), VBool(b)) => true
    case (VString(s), VErr(e)) => true
    case (VBool(b1), VBool(b2)) => !b1 || b1 == b2// false < true
    case (VBool(b), VDouble(d)) => false
    case (VBool(b), VString(s)) => false
    case (VBool(b), VErr(e)) => true
    case (VErr(e), _) => false
    case _ => true
  }

  def compareOption(a: Option[Value], b: Option[Value]) = (a, b) match {
    case (Some(v1), Some(v2)) => compare(v1, v2)
    case (Some(v1), None) => true
    case (None, Some(v2)) => false
    case _ => true
  }

  implicit class SheetSorter(val sheet: Sheet) extends AnyVal {

    def sort(x: Int, ascending: Boolean = true): Sheet = {

      // Get a map for row mutations. Map(0 -> 3), means row 0 becomes row 3
      val rowMutations = List.range(0, sheet.rows)
        .map(y => {
          val index = (x, y)
          (index, sheet.values.get(index))
        })
        .sortWith({
          case ((_, a), (_, b)) => compareOption(a, b)
        })
        .zip(List.range(0, sheet.rows))
        .map({
          case (((_, y1), _), y2) => (y1, y2)
        })
        .toMap

      // reposition all the cells
      // TODO: Update the references in the formulas, for example: A1 => A10
      val cells = sheet.cells
        .map({
          case ((x, y), cell) => ((x, rowMutations.get(y).get), cell)
        })

      // Move the values to the new positions
      val values = sheet.values
        .map({
          case ((x, y), v) => ((x, rowMutations.get(y).get), v)
        })

      // update dependents
      val dependents = sheet.dependents
        .map({
          case ((x, y), deps) => (
            (x, rowMutations.get(y).get),
            deps.map(a => (a._1, rowMutations.get(a._2))))
        })

      // move styles
      val styles = sheet.styles
        .map({
          case ((x, y), s) => ((x, rowMutations.get(y).get), s)
        })

      new Sheet(cells, values, sheet.dependents, sheet.styles)
    }


  }


}
