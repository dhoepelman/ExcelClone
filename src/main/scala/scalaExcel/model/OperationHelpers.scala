package scalaExcel.model

import rx.lang.scala.Observable

import scalaExcel.CellPos

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

}

