package scalaExcel.GUI.model

import rx.lang.scala.subjects.{PublishSubject, BehaviorSubject}
import scalafx.beans.property.ObjectProperty
import scalaExcel.GUI.util.AwaitingEvaluation
import scalaExcel.GUI.controller.Mediator
import rx.lang.scala.Subscription

class ObservableSheetCell(row: Int, column: Int, cell_ : SheetCell) extends ObjectProperty(cell_, "cell", cell_) {
  val firstValue = if (cell_ == null) null else cell_.evaluated
  val valueEmitter = BehaviorSubject[List[(Set[(Int, Int)], Any)]](List((Set((row, column)), firstValue)))
  valueEmitter.subscribe({
    cells => {
      cells.map({
        tuple => println("Emitted change " + tuple)
      })
    }
  })
  value = SheetCell.newEmpty()
  onChange({
    (_, oldValue, newValue) => {
      println("Observable changed from " + {
        if (oldValue == null) "null" else oldValue.verboseString
      } + " to " + newValue.verboseString)
    }
  })
}
