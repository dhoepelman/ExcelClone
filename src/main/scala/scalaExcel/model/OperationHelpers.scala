package scalaExcel.model

import rx.lang.scala.Observable

import scalaExcel.CellPos
import scalaExcel.formula.DependencyModifier.changeDependency

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

  /**
   * Implicit class enabling a sheet to permute its rows/columns
   * @param sheet the target sheet
   */
  implicit class PermutableSheet(val sheet: Sheet) {
    /**
     * Applys a set of row/column permutations on the target sheet
     * @param onRows        boolean value specifying if the permutations apply
     *                      to row indexes
     * @param permutations  the map of permutations of the nature
     *                      (index_before -> index_after)
     * @return              the modified target sheet
     */
    def applyPermutations(onRows: Boolean, permutations: Map[Int, Int]) = {

      def reposition(pos: CellPos) = pos match {
        case (x, y) =>
          if (onRows)
            (x, permutations.getOrElse(y, y))
          else
            (permutations.getOrElse(x, x), y)
      }

      def moveProperty[T](item: (CellPos, T)): (CellPos, T) = item match {
        case (pos, property) =>
          (reposition(pos),
            property match {
              case cell: Cell =>
                Cell(changeDependency(reposition)(cell.AST)).asInstanceOf[T]
              case deps: List[CellPos] =>
                deps.map(reposition).asInstanceOf[T]
              case other => other
            })
      }

      // reposition all the cells
      val cells = sheet.cells.map(moveProperty)

      // move the values to the new positions
      val values = sheet.values.map(moveProperty)

      // update dependents
      val dependents = sheet.dependents.map(moveProperty)

      // move styles
      val styles = sheet.styles.map(moveProperty)

      // return modified sheet
      new Sheet(cells, values, dependents, styles)
    }
  }

}

