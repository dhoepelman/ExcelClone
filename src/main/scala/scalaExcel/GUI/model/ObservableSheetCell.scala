package scalaExcel.GUI.model

import rx.lang.scala.subjects.BehaviorSubject
import scalafx.beans.property.ObjectProperty
import scalaExcel.GUI.controller.Mediator

class ObservableSheetCell(row: Int, column: Int, cell_ : SheetCell) extends ObjectProperty(cell_, "cell", cell_) {
  val subject = BehaviorSubject[List[(Int, Int, SheetCell)]](List((row, column, cell_)))
  subject.subscribe({
    cells => {
      cells.map({
        tuple => println("Emitted change " + tuple)
      })
    }
  })
  onChange({
    (_, oldValue, newValue) => {
      println("Observable changed from " + {
        if (oldValue == null) "null" else oldValue.verboseString
      } + " to " + {
        if (newValue == null) "null" else newValue.verboseString
      })
      if (newValue != null && newValue.evaluated == null)
        Mediator.changeCellExpr((row, column), newValue.expr)
      else
        subject.onNext(List((row, column, newValue)))
    }
  })
}
