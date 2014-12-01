package scalaExcel.model

import rx.lang.scala.Observable

object OperationHelpers {

  // Shortcuts, to quickly get values from the sheet observable
  implicit class ExtendedObservableSheet(val sheet: Observable[Sheet]) extends AnyVal {
    def filterCellValueAt(pos : CellPos) =
      sheet
        .map(_.valueAt(pos))
        .filter(!_.isEmpty)
        .map(_.get)
        .distinctUntilChanged
  }

}

