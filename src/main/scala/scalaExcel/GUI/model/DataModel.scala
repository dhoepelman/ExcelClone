package scalaExcel.GUI.model

import scalaExcel.GUI.model.DataModelFactory.DataTable
import scalaExcel.GUI.util.CircularEvaluation
import rx.lang.scala.{Subscription, Subject}

class DataModel() {
  private val defaultData = List(List("Cell11", "Cell12"), List("Cell21", "Cell22"))

  private val _dataTable = DataModelFactory.buildDefaultDataTable

  def dataTable = _dataTable

  private def populateDataTable(data: List[List[String]]) =
    data.view.zipWithIndex.foreach {
      case (row, i) => row.view.zipWithIndex.foreach {
        case (expr, j) => changeCellExpr((i, j), expr)
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

  def changeCellExpr(index: (Int, Int), expr: String): SheetCell = {
    println("Cell " + index + " changing expression to " + expr)
    val subscription = new SheetCellSubscriber(this, expr, index).subscription
    val observable = getCellObservable(index)
    val oldCell = observable.value
    if (oldCell != null) {
      val oldSubscription = oldCell.subscription
      if (oldSubscription != null && oldSubscription != subscription)
        oldSubscription.unsubscribe()
    }
    val cell = SheetCell.modifySubscription(oldCell, subscription)
    observable.value = cell
    cell
  }

  def propagateChange(previousEmitters: Set[(Int, Int)], newValue: SheetCell, index: (Int, Int), subject: Subject[List[(Set[(Int, Int)], SheetCell)]]) {
    newValue.evaluated match {
      case x: CircularEvaluation => Unit
      case _ =>
        val emitters = if (previousEmitters == null) Set(index) else previousEmitters + index
        subject.onNext(List((emitters, newValue)))
    }
  }

  def cellEvaluated(index: (Int, Int), expr: String, value: Any, previousEmitters: Set[(Int, Int)]) = {
    println("Cell " + index + " evaluated to " + value)
    val observable = getCellObservable(index)
    val newValue = SheetCell.markEvaluated(observable.value, expr, value)
    observable.value = newValue
    propagateChange(previousEmitters, newValue, index, observable.valueEmitter)
  }

  def changeCellStylist(index: (Int, Int), stylist: Any => String) = {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifyStylist(observable.value, stylist)
  }

  def changeCellFormatter(index: (Int, Int), formatter: Any => String) = {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifyFormatter(observable.value, formatter)
  }

  def portCellSubscription(index: (Int, Int), cell: SheetCell, subscription: Subscription) {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifySubscription(cell, subscription)
  }
}
