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
    case (VBool(b1), VBool(b2)) => !b1 || b1 == b2 // false < true
    case (VBool(b), VDouble(d)) => false
    case (VBool(b), VString(s)) => false
    case (VBool(b), VErr(e)) => true
    case (VErr(e), _) => false
    case _ => true
  }

  implicit class SheetSorter(val sheet: Sheet) extends AnyVal {

    def sort(x: Int, ascending: Boolean = true): Sheet = {

      val rows = sheet.rows

      type CellValue = (CellPos, Option[Value])
      def ascOrDescCompare(a: CellValue, b: CellValue) = (a, b) match {
        case ((_, Some(a)), (_, Some(b))) => {
          val cmp = compare(a, b)
          if (ascending) cmp else !cmp
        }
        case ((_, Some(_)), (_, None)) => true
        case ((_, None), (_, Some(_))) => false
        case ((_, None), (_, None)) => false
      }

      // Get a map for row mutations. Map(0 -> 3), means row 0 becomes row 3
      val rowMutations =
        (0 to rows - 1)
        .map { y =>
          ((x, y), sheet.values.get((x, y)))
        }
        .sortWith(ascOrDescCompare)
        .zip(0 to rows - 1)
        .map {
          case (((_, y1), _), y2) => (y1, y2)
        }
        .toMap

      val cellModifier = DependencyModifier.changeDependencyRows(rowMutations)

      // reposition all the cells
      // TODO: Update the references in the formulas, for example: A1 => A10
      val cells =
        sheet.cells
        .map {
          case ((x, y), cell) => {
            val p2 = (x, rowMutations.get(y).get)
            (p2, Cell(p2, cellModifier(cell.AST)))
          }
        }

      // Move the values to the new positions
      val values =
        sheet.values
        .map {
          case ((x, y), v) => ((x, rowMutations.get(y).get), v)
        }

      // update dependents
      val dependents =
        sheet.dependents
        .map {
          case ((x, y), deps) => (
            (x, rowMutations.get(y).getOrElse(y)),
            deps.map(a => (a._1, rowMutations.get(a._2).getOrElse(a._2))))
        }

      // move styles
      val styles =
        sheet.styles
        .map {
          case ((x, y), s) => ((x, rowMutations.get(y).get), s)
        }

      new Sheet(cells, values, dependents, styles)
    }

  }

}
