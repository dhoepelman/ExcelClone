package scalaExcel.GUI.modelwrapper

import rx.lang.scala.{Subject, Observable}
import scalaExcel.GUI.util.{CircularEvaluation, MockParser}

class SheetCellEvaluator(model_ : DataModel, expression_ : String, index_ : (Int, Int)) {

  private def composeSubjects(subjects: List[Subject[List[(Set[(Int, Int)], Any)]]]): Observable[List[(Set[(Int, Int)], Any)]] =
    subjects match {
      case List(s1) => s1
      case List(s1, s2) => s1.combineLatestWith(s2)((x, y) => x ++ y)
      case ss => composeSubjects(ss.init).combineLatestWith(ss.last)((x, y) => x ++ y)
    }

  val index = index_
  val expression = expression_

  // parse new expression to get the references and evaluator function
  private val (refs, evaluator) = MockParser.parse(expression)

  // compose cell subjects with combineLatest operator;
  // make the final observable connectable to defer subscription
  val derivedObservable = refs match {
    case List() => null
    case l => composeSubjects(refs.map(x => model_.getCellObservable(x).valueEmitter)).publish
  }

  private val changeInterpreter = (changes: List[(Set[(Int, Int)], Any)]) => {
    // combine all the indexes of the cells that contributed changes
    val (previousEmitters, values) = changes.foldLeft(Set[(Int, Int)](), List[Any]()) {
      (partial, change) =>
        (partial._1 ++ change._1, partial._2 :+ change._2)
    }
    // if this cell's index is among them, a loop has been produced
    if (previousEmitters.contains(index))
      model_.cellEvaluated(this, new CircularEvaluation(expression), null)
    else
      model_.cellEvaluated(this, evaluator(values), previousEmitters)
  }

  //register interpreter to the composed observable
  val subscription = derivedObservable match {
    case null => null
    case obs => obs.subscribe(changeInterpreter)
  }

  // if the cell does not reference others, evaluate it now
  if (refs == List())
    model_.cellEvaluated(this, evaluator(List()), null)

}
