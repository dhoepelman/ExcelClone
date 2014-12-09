package scalaExcel.model

import scalaExcel.model.OperationHelpers._
import scalaExcel.CellPos

object Slider {

  implicit class SheetSlider(val sheet: Sheet) extends AnyVal {

    def slide(forward: Boolean, onRows: Boolean, offset: Int, startIndex: Int): (Sheet, List[CellPos]) = {
      val relativeOffset = if(forward) offset else -offset
      val stopIndex =  if(onRows) sheet.rows else sheet.cols
      val permutations = List.range(startIndex, stopIndex)
        .map(i => i -> (i+relativeOffset))
        .toMap
      val (newSheet, updates) =
        if(forward)
          (sheet, List())
        else {
          val toBeDeleted =
            List.range(startIndex, startIndex + offset)
            .flatMap(i =>
              if(onRows)
                List.range(0, sheet.cols).map(c => (c, i))
              else
                List.range(0, sheet.rows).map(r => (i, r)))
          println(toBeDeleted)
          toBeDeleted.foldLeft((sheet, List[CellPos]()))({
            case ((crtSheet, crtUpdates), pos) => crtSheet.deleteCell(pos)
          })
        }
      println(newSheet.cells)
      (newSheet.applyPermutations(onRows, permutations), updates)
    }
  }
}
