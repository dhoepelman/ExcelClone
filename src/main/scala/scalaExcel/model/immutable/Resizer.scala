package scalaExcel.model.immutable

import scalaExcel.model.immutable.OperationHelpers._

object Resizer {

  /**
   * Enables a sheet to slide its contents from a given
   * row/column index onward by a given offset
   *
   * To be used when adding/removing rows/columns
   * @param sheet the target sheet
   */
  implicit class SheetResizer(val sheet: Sheet) extends AnyVal {

    private def mapOffset(from: Int, to: Int, offset: Int) =
      List.range(from, to).map(i => i -> (i + offset)).toMap

    private def clearSheet(sheet: Sheet, startCol: Int, cols: Int, startRow: Int, rows: Int) =
      // calculate all positions that should be cleared
      ( for {x <- List.range(startCol, startCol + cols)
             y <- List.range(startRow, startRow + rows)}
        yield (x, y)
      )
      // delete the cells at those positions
      .foldLeft(sheet)({
        case (crtSheet, pos) => crtSheet.deleteCell(pos)
      })

    /**
     * Adds 'count' columns to the sheet, starting at column 'startIndex'
     * @param count       the number of columns to add
     * @param startIndex  the smallest index of the added columns
     * @return            the modified target sheet
     */
    def addColumns(count: Int, startIndex: Int): Sheet =
      sheet.applyColumnPermutations(mapOffset(startIndex, sheet.cols, count))

    /**
     * Adds 'count' rows to the sheet, starting at row 'startIndex'
     * @param count       the number of rows to add
     * @param startIndex  the smallest index of the added rows
     * @return            the modified target sheet
     */
    def addRows(count: Int, startIndex: Int): Sheet =
      sheet.applyRowPermutations(mapOffset(startIndex, sheet.rows, count))

    /**
     * Removes 'count' columns to the sheet, starting at column 'startIndex'
     * @param count       the number of columns to remove
     * @param startIndex  the smallest index of the removed columns
     * @return            the modified target sheet
     */
    def removeColumns(count: Int, startIndex: Int): Sheet = {
      // mark all references to these rows as invalid
      val invalidatedSheet = sheet.removeReferencesToColumns(List.range(startIndex, startIndex + count))

      // the columns first need to be cleared
      // since the the overwriting cells might not contain information
      val clearedSheet = clearSheet(invalidatedSheet, startIndex, count, 0, sheet.rows)

      clearedSheet.applyColumnPermutations(mapOffset(startIndex, sheet.cols, -count))
    }

    /**
     * Removes 'count' rows to the sheet, starting at row 'startIndex'
     * @param count       the number of rows to remove
     * @param startIndex  the smallest index of the removed rows
     * @return            the modified target sheet
     */
    def removeRows(count: Int, startIndex: Int): Sheet = {
      // mark all references to these rows as invalid
      val invalidatedSheet = sheet.removeReferencesToRows(List.range(startIndex, startIndex + count))

      // the rows first need to be cleared
      // since the the overwriting cells might not contain information
      val clearedSheet = clearSheet(invalidatedSheet, 0, sheet.cols, startIndex, count)

      // return the sheet with repositioned cells
      clearedSheet.applyRowPermutations(mapOffset(startIndex, sheet.rows, -count))
    }

  }

}
