package scalaExcel.GUI.model

import scalaExcel.GUI.model.DataModelFactory.DataTable
import scalaExcel.GUI.util.{CircularEvaluation, ErroneousEvaluation}
import rx.lang.scala.Subject

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

  def changeCellExpr(index: (Int, Int), expr: String) = {
    println("Cell " + index + " changing expression to " + expr)
    new SheetCellSubscriber(this, expr, index)
  }

  def propagateChange(newValue: SheetCell, index: (Int, Int), subject: Subject[List[(Int, Int, SheetCell)]]) {
    newValue.evaluated match {
      case x: CircularEvaluation => Unit
      case _ => subject.onNext(List((index._1, index._2, newValue)))
    }
  }

  def cellEvaluated(index: (Int, Int), expr: String, value: Any, subscriber: SheetCellSubscriber) = {
    println("Cell " + index + " evaluated to " + value + " with subs " + subscriber.subscription)
    val observable = getCellObservable(index)
    if (observable.value != null) {
      val oldSubscription = observable.value.subscription
      if (oldSubscription != null && oldSubscription != subscriber.subscription)
        oldSubscription.unsubscribe()
    }
    val newValue = SheetCell.markEvaluated(index, observable.value, expr, value, subscriber.subscription)
    observable.value = newValue
    propagateChange(newValue, index, observable.subject)
  }

  def changeCellStylist(index: (Int, Int), stylist: Any => String) = {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifyStylist(index, observable.value, stylist)
  }

  def changeCellFormatter(index: (Int, Int), formatter: Any => String) = {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifyFormatter(index, observable.value, formatter)
  }
}
