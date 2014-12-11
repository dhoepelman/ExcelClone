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
   * Transforms two maps of row/column permutations into a cell repositioning
   * function  of the form (CellPos) => CellPos
   * @param rowPermutations map containing row permutations of the nature
                            (index_before -> index_after)
   * @param colPermutations map containing column permutations of the nature
                            (index_before -> index_after)
   * @return                the cell repositioning function of the form
   *                        (CellPos) => CellPos
   */
  def getCellRepositioner(colPermutations: Map[Int, Int] = Map[Int, Int](),
                          rowPermutations: Map[Int, Int] = Map[Int, Int]()): (CellPos) => CellPos = {
    case (x, y) =>
      (colPermutations.getOrElse(x, x), rowPermutations.getOrElse(y, y))
  }

  /**
   * Enables a sheet to reposition its cells
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
      applyRepositioning(getCellRepositioner(rowPermutations = permutations))

    def applyColumnPermutations(permutations: Map[Int, Int]) =
      applyRepositioning(getCellRepositioner(colPermutations = permutations))

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
              case posList: List[Any] =>
                posList.map(p => repositioner(p.asInstanceOf[CellPos])).asInstanceOf[T]
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

