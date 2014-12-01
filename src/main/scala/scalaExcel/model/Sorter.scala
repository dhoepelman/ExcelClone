package scalaExcel.model

import scalaExcel.formula._
import scalaExcel.CellPos

object Sorter {

  implicit class SheetSorter(val sheet: Sheet) extends AnyVal {

    def sort(x: Int, ascending: Boolean = true): Sheet = {

      val rows = sheet.rows

      type CellValue = (CellPos, Option[Value])
      def ascOrDescCompare(a: CellValue, b: CellValue) = (a, b) match {
        case ((_, Some(a)), (_, Some(b))) => {
          (a.compare(b) * (if (ascending) 1 else -1)) < 0
        }
        case ((_, None), (_, _)) => false
        case ((_, _), (_, None)) => true
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
      val cells =
        sheet.cells
        .map {
          case ((x, y), cell) => {
            val p2 = (x, rowMutations.get(y).get)
            (p2, Cell(cellModifier(cell.AST)))
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
          case ((x, y), s) => ((x, rowMutations.get(y).getOrElse(y)), s)
        }

      new Sheet(cells, values, dependents, styles)
    }

  }

}
