package scalaExcel.model

import scalaExcel.model.OperationHelpers._

object Slider {

  implicit class SheetSlider(val sheet: Sheet) extends AnyVal {

    def slide(forward: Boolean, onRows: Boolean, offset: Int, startIndex: Int): Sheet = {
      val relativeOffset = if(forward) offset else -offset
      val stopIndex =  if(onRows) sheet.rows else sheet.cols
      val permutations = List.range(startIndex, stopIndex)
        .map(i => i -> (i+relativeOffset))
        .toMap
//      if(!forward) {
//        val toBeDeleted =
//          List.range(startIndex+relativeOffset, stopIndex+relativeOffset)
//          .flatMap(i =>
//            if(onRows)
//              List.range(0, sheet.cols).map(c => (c, i))
//            else
//              List.range(0, sheet.rows).map(r => (i, r)))
//        toBeDeleted.foldLeft(sheet)((sheet, pos) => sheet.deleteCell(pos))
//      }
      sheet.applyPermutations(onRows, permutations)
    }
  }
}
