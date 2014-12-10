package scalaExcel.model

import scalaExcel.model.OperationHelpers._
import scalaExcel.CellPos

object Slider {

  /**
   * Implicit class enabling a sheet to slide its contents from a given
   * row/column index onward by a given offset
   *
   * To be used when adding/removing rows/columns
   * @param sheet the target sheet
   */
  implicit class SheetSlider(val sheet: Sheet) extends AnyVal {

    /**
     * Slides the target sheet's contents
     * @param forward     slide direction (true for add, false for remove)
     * @param onRows      slide orientation (true for rows, false for columns)
     * @param offset      the number of rows/columns to slide over
     * @param startIndex  the smallest index of the rows/columns which are to
     *                    be moved
     * @return            the modified target sheet and the positions of the cells
     *                    that need to update their values as a result
     */
    def slide(forward: Boolean, onRows: Boolean, offset: Int, startIndex: Int): (Sheet, List[CellPos]) = {

      // calculate row/column permutations according to slide orientation and direction
      val permutations =
        List.range(startIndex, if (onRows) sheet.rows else sheet.cols)
        .map(i => i -> (if (forward) i + offset else i - offset))
        .toMap

      // transform the permutations into a repositioning function
      val repositioner = permutations.asCellRepositioner(onRows)

      // prepare the sheet
      val (newSheet, updates) =
        if(forward)
          // if rows/columns are to be added, there is no pre-processing needed
          (sheet, List())
        else {
          /**
           * If removal is intended, the rows/columns first need to be cleared
           * This is required since the the overwriting cells might not contain
           * information (thus not exist)
           */

          // calculate all positions that should be cleared
          val toBeDeleted =
            List.range(startIndex, startIndex + offset)
            .flatMap(i =>
              if(onRows)
                List.range(0, sheet.cols).map(c => (c, i))
              else
                List.range(0, sheet.rows).map(r => (i, r)))
          // delete the cells at those positions
          toBeDeleted.foldLeft((sheet, List[CellPos]()))({
            case ((crtSheet, crtUpdates), pos) =>
              val (s, u) = crtSheet.deleteCell(pos)
              (s, crtUpdates ++
                // remember the cells that depend on the deleted cells
                // but remove from them the cells that are also being deleted
                // and update their position according to the permutations
                u.filterNot(toBeDeleted.contains).map(repositioner))
          })
        }

      // return the sheet with repositioned cells
      // and the updates that need to be processed as a result of deletion
      (newSheet.applyRepositioning(repositioner), updates)
    }
  }
}
