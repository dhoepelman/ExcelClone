package scalaExcel.model

import scalaExcel.formula._
import scalaExcel.CellPos
import scalaExcel.model.OperationHelpers._

object Sorter {

  implicit class SheetSorter(val sheet: Sheet) extends AnyVal {

    def sort(x: Int, ascending: Boolean = true): Sheet = {

      val rows = sheet.rows

      type CellValue = (CellPos, Option[Value])
      def ascOrDescCompare(va: CellValue, vb: CellValue) = (va, vb) match {
        case ((_, Some(a)), (_, Some(b))) =>
          (a.compare(b) * (if (ascending) 1 else -1)) < 0
        case ((_, None), (_, _)) => false
        case ((_, _), (_, None)) => true
      }

      // Get a map for row mutations. Map(0 -> 3), means row 0 becomes row 3
      val rowMutations =
        (0 to rows - 1)
        .map { y =>
          ((x, y), sheet.valueAt((x, y)))
        }
        .sortWith(ascOrDescCompare)
        .zip(0 to rows - 1)
        .map {
          case (((_, y1), _), y2) => (y1, y2)
        }
        .toMap

      sheet.applyRowPermutations(rowMutations)
    }
  }

}
