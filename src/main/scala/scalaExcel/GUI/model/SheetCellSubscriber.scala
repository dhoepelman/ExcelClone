package scalaExcel.GUI.model

import rx.lang.scala.{Subscription, Subject, Observable}
import scalaExcel.GUI.util.{CircularEvaluation, MockParser}
import rx.lang.scala.schedulers.TrampolineScheduler

class SheetCellSubscriber(model_ : DataModel, expr_ : String, index_ : (Int, Int)) {
  println("Instantiating subscriber for " + expr_)
  private val combiner = (x: List[(Set[(Int, Int)], SheetCell)], y: List[(Set[(Int, Int)], SheetCell)]) => x ++ y

  val (refs, evaluator) = MockParser.parse(expr_)

  def changeReducer(partial: (Set[(Int, Int)], List[Any]), change: (Set[(Int, Int)], SheetCell)): (Set[(Int, Int)], List[Any]) = {
    (partial._1 ++ change._1, partial._2 :+ change._2.evaluated)
  }

  private val transformer = (changes: List[(Set[(Int, Int)], SheetCell)]) => {
    val (previousEmitters, values) = changes.foldLeft(Set[(Int, Int)](), List[Any]())(changeReducer)
    if (previousEmitters.contains(index_)) {
      println("Circular dependency for " + index_ + "!")
      model_.cellEvaluated(index_, expr_, new CircularEvaluation(expr_), null)
    } else {
      val value = evaluator(values)
      println("Value of " + index_ + " updated to " + value)
      model_.cellEvaluated(index_, expr_, value, previousEmitters)
    }
  }

  val subscription: Subscription = {
    println("Subscribing " + index_ + " for " + refs)
    if (refs == List()) {
      model_.cellEvaluated(index_, expr_, evaluator(List()), null)
      null
    } else {
      val subjects = refs.map(x => model_.getCellObservable(x).valueEmitter)
      val obs = composeSubjects(subjects)
      obs.subscribeOn(TrampolineScheduler()).subscribe(transformer)
    }
  }

  private def composeSubjects(subjects: List[Subject[List[(Set[(Int, Int)], SheetCell)]]]): Observable[List[(Set[(Int, Int)], SheetCell)]] = subjects match {
    case List(s1) => s1
    case List(s1, s2) => s1.combineLatestWith(s2)(combiner)
    case ss => composeSubjects(ss.init).combineLatestWith(ss.last)(combiner)
  }
}
