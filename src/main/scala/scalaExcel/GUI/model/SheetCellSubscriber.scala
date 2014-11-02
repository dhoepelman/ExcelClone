package scalaExcel.GUI.model

import rx.lang.scala.{Subscription, Subject, Observable}
import scalaExcel.GUI.util.MockParser

class SheetCellSubscriber(model_ : DataModel, expr_ : String, index_ : (Int, Int)) {

  private val combiner = (x: List[(Int, Int, SheetCell)], y: List[(Int, Int, SheetCell)]) => x ++ y

  val (refs, evaluator) = MockParser.parse(expr_)

  //TODO unnecessary if evaluator is coordinate agnostic
  private val transformer = (changes: List[(Int, Int, SheetCell)]) => {
    val value = evaluator(changes.map(change =>
      if (change._3 == null) null else change._3.evaluated))
    println("Value of " + index_ + " updated to " + value)
    model_.cellEvaluated(index_, expr_, value, this)
  }

  val subscription : Subscription = {
    println("Subscribing " + index_ + " for " + refs)
    if (refs == List()) {
      model_.cellEvaluated(index_, expr_, evaluator(List()), this)
      null
    } else {
      val subjects = refs.map(x => model_.getCellObservable(x).subject)
      val obs = compose(subjects)
      obs.subscribe(transformer)
    }
  }

  private def compose(subjects: List[Subject[List[(Int, Int, SheetCell)]]]): Observable[List[(Int, Int, SheetCell)]] = subjects match {
    case List(s1) => s1
    case List(s1, s2) => s1.combineLatestWith(s2)(combiner)
    case ss => compose(ss.init).combineLatestWith(ss.last)(combiner)
  }
}
