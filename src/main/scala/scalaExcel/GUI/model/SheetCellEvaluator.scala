package scalaExcel.GUI.model

import rx.lang.scala.{Subject, Observable}
import scalaExcel.GUI.util.{CircularEvaluation, MockParser}

class SheetCellEvaluator(model_ : DataModel, expression_ : String, index_ : (Int, Int)) {

  private def composeSubjects(subjects: List[Subject[List[(Set[(Int, Int)], Any)]]]): Observable[List[(Set[(Int, Int)], Any)]] =
    subjects match {
      case List(s1) => s1
      case List(s1, s2) => s1.combineLatestWith(s2)((x, y) => x ++ y)
      case ss => composeSubjects(ss.init).combineLatestWith(ss.last)((x, y) => x ++ y)
    }

  println("Instantiating subscriber for " + expression_)
  val index = index_
  val expression = expression_

  private val (refs, evaluator) = MockParser.parse(expression)

  val derivedObservable = refs match {
    case List() => null
    case l => composeSubjects(refs.map(x => model_.getCellObservable(x).valueEmitter)).publish
  }

  private val changeInterpreter = (changes: List[(Set[(Int, Int)], Any)]) => {
    val (previousEmitters, values) = changes.foldLeft(Set[(Int, Int)](), List[Any]()) {
      (partial, change) =>
        (partial._1 ++ change._1, partial._2 :+ change._2)
    }
//    if (previousEmitters.contains(index)) {
//      println("Circular dependency for " + index_ + "!")
//      model_.cellEvaluated(index_, expression_, new CircularEvaluation(expression), null)
//    } else {
      val value = evaluator(values)
      println("Value of " + index + " updated to " + value)
      model_.cellEvaluated(this, value, previousEmitters)
//    }
  }

  val subscription = derivedObservable match {
    case null => null
    case obs => obs.subscribe(changeInterpreter)
  }

  if (refs == List())
    model_.cellEvaluated(this, evaluator(List()), null)

}
