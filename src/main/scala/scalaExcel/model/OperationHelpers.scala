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
   * Implicit class enabling a map containing permutations of the nature
   * (index_before -> index_after) to become a cell repositioning function
   * of the form (CellPos) => CellPos
   * @param permutations  the map with permutations
   */
  implicit class CellRepositioningMap(val permutations: Map[Int, Int]) {
    /**
     * Uses the map as a set of row/column permutations to be applied on a
     * sheet
     * @param onRows        boolean value specifying if the permutations apply
     *                      to the sheet's row indexes
     * @return              the cell repositioning function of the form
     *                      (CellPos) => CellPos
     */
    def asCellRepositioner(onRows: Boolean): (CellPos) => CellPos = {
      case (x, y) =>
        if (onRows)
          (x, permutations.getOrElse(y, y))
        else
          (permutations.getOrElse(x, x), y)
      }
  }

  /**
   * Implicit class enabling a sheet to reposition its cells
   * @param sheet the target sheet
   */
  implicit class PermutableSheet(val sheet: Sheet) {

    /**
     * Applies row permutations to the target sheet
     * @param permutations  the map of permutations of the nature
     *                      (index_before -> index_after)
     * @return              the permuted target sheet
     */
    def applyRowPermutations(permutations: Map[Int, Int]) =
      applyRepositioning(permutations.asCellRepositioner(onRows = true))

    def applyColumnPermutations(permutations: Map[Int, Int]) =
      applyRepositioning(permutations.asCellRepositioner(onRows = false))

    /**
     * Applies custom repositioning on the cells of the target sheet
     * @param repositioner  the cell repositioning function of the form
     *                      (CellPos) => CellPos
     * @return              the modified target sheet
     */
    def applyRepositioning(repositioner: (CellPos) => CellPos) = {

      def moveProperty[T](item: (CellPos, T)): (CellPos, T) = item match {
        case (pos, property) =>
          (repositioner(pos),
            property match {
              case cell: Cell =>
                Cell(changeDependency(repositioner)(cell.AST)).asInstanceOf[T]
              case posList: List[CellPos @unchecked] =>
                posList.map(repositioner).asInstanceOf[T]
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

