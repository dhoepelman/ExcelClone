package scalaExcel.GUI.model

import rx.lang.scala.subjects.BehaviorSubject
import scalafx.beans.property.ObjectProperty
import scalaExcel.GUI.util.AwaitingEvaluation
import scalaExcel.GUI.controller.Mediator
import rx.lang.scala.Subscription

class ObservableSheetCell(row: Int, column: Int, cell_ : SheetCell) extends ObjectProperty(cell_, "cell", cell_) {
  val valueEmitter = BehaviorSubject[List[(Set[(Int, Int)], SheetCell)]](List((Set((row, column)), cell_)))
  valueEmitter.subscribe({
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
//      if (oldValue != null && newValue.expr == oldValue.expr && newValue.evaluated.isInstanceOf[AwaitingEvaluation]) {
//        println("Reverting to " + oldValue)
//        Mediator.portCellSubscription((row, column), oldValue, newValue.subscription)
//      }
    }
  })
}
