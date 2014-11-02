package scalaExcel.GUI.util

import rx.lang.scala.{Subscription, Subject, Observable}
import scalaExcel.GUI.controller.Mediator
import scalaExcel.GUI.model.SheetCell

class CellExprParser(expr_ : String, location_ : (Int, Int), cell_ : SheetCell) {

  val (refs, evaluator) = parse

  val combiner = (x: List[(Int, Int, SheetCell)], y: List[(Int, Int, SheetCell)]) => x ++ y

  //TODO unnecessary if evaluator is coordinate agnostic
  val transformer = (changes: List[(Int, Int, SheetCell)]) => {
    val value = evaluator(changes.map(change => change._3.evaluated))
    println("Value of " + location_ + " updated to " + value)
    Mediator.cellEvaluated(location_, value)
  }

  def compose(subjects: List[Subject[List[(Int, Int, SheetCell)]]]): Observable[List[(Int, Int, SheetCell)]] = subjects match {
    case List(s1) => s1
    case List(s1, s2) => s1.combineLatestWith(s2)(combiner)
    case ss => compose(ss.init).combineLatestWith(ss.last)(combiner)
  }

  def getSubscription: Subscription = {
    println("Subscribing " + location_ + " for " + refs)
    if (refs == List()) {
      cell_.evaluated = evaluator(List())
      return null
    }
    val subjects = refs.map(x => Mediator.getCellObservable(x).subject)
    val obs = compose(subjects)
    obs.subscribe(transformer)
  }

  def parse: (List[(Int, Int)], (List[Any]) => Any) = {
    //TODO actual parsing
    val refList = if (location_ ==(1, 1)) List((0, 0), (1, 0), (0, 1)) else List()
    (refList, {
      (values: List[Any]) => {
        if (values == List()) expr_ else values.mkString("+")
      }
    })
  }
}
