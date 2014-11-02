package scalaExcel.GUI.model

import scalaExcel.GUI.model.DataModelFactory.DataTable
import rx.lang.scala.Subscription

class DataModel() {
  private val defaultData = List(List("Cell11", "Cell12"), List("Cell21", "Cell22"))

  private val dataTable = DataModelFactory.buildDefaultDataTable

  private def populateDataTable(data: List[List[String]]) =
    data.view.zipWithIndex.foreach {
      case (row, i) => row.view.zipWithIndex.foreach {
        case (expr, j) => changeCellExpr((i, j), expr)
      }
    }

  private def populateDataTableWithDefault(raw: DataTable): Unit =
    populateDataTable(defaultData)

  def getCellObservable(index: (Int, Int)): ObservableSheetCell = dataTable.get(index._1).get(index._2)

  def getCell(index: (Int, Int)): SheetCell = getCellObservable(index).value

  def getCellValue(index: (Int, Int)): Any = getCell(index).evaluated

  def getDataTable = dataTable

  def populateDataModel(data: List[List[String]]) =
    if (data == null)
      populateDataTableWithDefault(dataTable)
    else
      populateDataTable(data)

  def changeCellExpr(index: (Int, Int), expr: String) = {
    println("Cell " + index + " changing expression to " + expr)
    new SheetCellSubscriber(this, expr, index)
  }

  def cellEvaluated(index: (Int, Int), expr: String, value: Any, subscriber: SheetCellSubscriber) = {
    println("Cell " + index + " evaluated to " + value + " with subs ")
    val observable = getCellObservable(index)
    if (observable.value != null) {
      val oldSubscription = observable.value.subscription
      if (oldSubscription != null)
        oldSubscription.unsubscribe()
    }
    observable.value = SheetCell.markEvaluated(index, observable.value, expr, value, subscriber.subscription)
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
