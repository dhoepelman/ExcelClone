package scalaExcel.model

import rx.lang.scala.Observable

import scalaExcel.CellPos
import scalaExcel.formula.DependencyModifier

object OperationHelpers {

  // Shortcuts, to quickly get values from the sheet observable
  implicit class ExtendedObservableSheet(val sheet: Observable[Sheet]) extends AnyVal {
    def filterCellValueAt(pos : CellPos) =
      sheet
        .map(_.valueAt(pos))
        .filter(_.nonEmpty)
        .map(_.get)
        .distinctUntilChanged
  }

  implicit class PermutableSheet(val sheet: Sheet) {
    def applyPermutations(onRows: Boolean, permutations: Map[Int, Int]) = {

      val cellModifier =
        DependencyModifier.changeDependencyRows(permutations)

      val reposition = (pos: CellPos) => pos match{
        case (x, y) =>
          if (onRows)
            (x, permutations.getOrElse(y, y))
          else
            (permutations.getOrElse(x, x), y)
      }

      // reposition all the cells
      val cells =
        sheet.cells
          .map {
          case (pos, cell) =>
            (reposition(pos), Cell(cellModifier(cell.AST)))
        }

      // Move the values to the new positions
      val values =
        sheet.values
          .map {
          case (pos, v) => (reposition(pos), v)
        }

      // update dependents
      val dependents =
        sheet.dependents
          .map {
          case (pos, deps) => (reposition(pos), deps.map(reposition))
        }

      // move styles
      val styles =
        sheet.styles
          .map {
          case (pos, s) => (reposition(pos), s)
        }

      new Sheet(cells, values, dependents, styles)
    }
  }

}

