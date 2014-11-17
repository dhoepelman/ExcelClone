package scalaExcel.GUI.modelwrapper

import scalaExcel.GUI.modelwrapper.DataModelFactory.DataTable
import scalaExcel.GUI.util.CircularEvaluation
import rx.lang.scala.Subject

class DataModel() {
  private val defaultData = List(List("Cell11", "Cell12"), List("Cell21", "Cell22"))

  private val _dataTable = DataModelFactory.buildDefaultDataTable

  def dataTable = _dataTable

  private def populateDataTable(data: List[List[String]]) =
    data.view.zipWithIndex.foreach {
      case (row, i) => row.view.zipWithIndex.foreach {
        case (expression, j) => changeCellExpression((i, j), expression)
      }
    }

  private def populateDataTableWithDefault(raw: DataTable): Unit =
    populateDataTable(defaultData)

  def getCellObservable(index: (Int, Int)): ObservableSheetCell = _dataTable.get(index._1).get(index._2)

  def getCell(index: (Int, Int)): SheetCell = getCellObservable(index).value

  def getCellValue(index: (Int, Int)): Any = getCell(index).evaluated

  def populateDataModel(data: List[List[String]]) =
    if (data == null)
      populateDataTableWithDefault(_dataTable)
    else
      populateDataTable(data)

  def changeCellExpression(index: (Int, Int), expression: String) = {
    println("Cell " + index + " changing expression to " + expression)
    // pass the interpretation to an evaluator instance
    val evaluator = new SheetCellEvaluator(this, expression, index)
    // start listening for changes on references
    if (evaluator.derivedObservable != null)
      evaluator.derivedObservable.connect
  }

  def propagateChange(previousEmitters: Set[(Int, Int)], index: (Int, Int), value: Any, emitter: Subject[List[(Set[(Int, Int)], Any)]]) {
    value match {
      case x: CircularEvaluation => Unit
      case x =>
        // add self to emitters
        val emitters = if (previousEmitters == null) Set(index) else previousEmitters + index
        emitter.onNext(List((emitters, x)))
    }
  }

  def cellEvaluated(evaluator: SheetCellEvaluator, value: Any, previousEmitters: Set[(Int, Int)]): Unit = {
    println("Cell " + evaluator.index + " evaluated to " + value)
    val observable = getCellObservable(evaluator.index)
    // unregister from previous subscription if it has changed
    val oldCell = observable.value
    if (oldCell != null) {
      val oldSubscription = oldCell.subscription
      if (oldSubscription != null && oldSubscription != evaluator.subscription)
        oldSubscription.unsubscribe()
    }
    // mark change
    observable.value = SheetCell.markEvaluated(observable.value, evaluator.expression, value, evaluator.subscription)
    // propagate change to other cells
    propagateChange(previousEmitters, evaluator.index, value, observable.valueEmitter)
  }

  def changeCellStylist(index: (Int, Int), stylist: Any => String) = {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifyStylist(observable.value, stylist)
  }

  def changeCellFormatter(index: (Int, Int), formatter: Any => String) = {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifyFormatter(observable.value, formatter)
  }

}
