package scalaExcel.GUI.model

import rx.lang.scala.subjects.BehaviorSubject
import scalafx.beans.property.ObjectProperty
import scalaExcel.GUI.controller.Mediator
import scalaExcel.GUI.util.{ErroneousEvaluation, AwaitingEvaluation}

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
      } + " to " + newValue.verboseString)
      newValue.evaluated match {
        case x: AwaitingEvaluation => Mediator.changeCellExpr((row, column), newValue.expr)
        case x: ErroneousEvaluation => Unit //TODO maybe dialog
        case _ => subject.onNext(List((row, column, newValue)))
      }
    }
  })
}
