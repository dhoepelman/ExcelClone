package scalaExcel.model

import rx.lang.scala.Observable

object OperationHelpers {

  // Shortcuts, to quickly get values from the sheet observable
  implicit class ExtendedObservableSheet(val sheet: Observable[Sheet]) extends AnyVal {
    def filterCellValueAt(x: Int, y: Int) =
      sheet
        .map(_.valueAt(x, y))
        .filter(!_.isEmpty)
        .map(_.get)
        .distinctUntilChanged
  }

}

