package scalaExcel.model

import scalaExcel.model.Cell

object Slider {

  implicit class SheetSlider(val sheet: Sheet) extends AnyVal {

    def slide(forward: Boolean, onRows: Boolean, offset: Int, startIndex: Int): Sheet = {
      sheet
    }
  }
}
