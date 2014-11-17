package scalaExcel.GUI.modelwrapper

import scalaExcel.GUI.modelwrapper.DataModelFactory.DataTable
import scalaExcel.model.Model

class DataModel() {
  private val defaultData = List(List("Cell11", "Cell12"), List("Cell21", "Cell22"))

  private val _dataTable = DataModelFactory.buildDefaultDataTable

  private val _immutableModel = new Model()

  _immutableModel.sheet.subscribe(sheet => {
    sheet.cells.foreach(x =>
      changeCell(x._1, x._2.f, sheet.valueAt(x._1._1, x._1._2) match {
        case Some(value) => value
        case _ => null
      })
    )
  })

  def dataTable = _dataTable

  private def populateDataTable(data: List[List[String]]) =
    populateImmutableModel(data)

  private def populateImmutableModel(data: List[List[String]]) =
    data.view.zipWithIndex.foreach {
      case (row, i) => row.view.zipWithIndex.foreach {
        case (expression, j) => _immutableModel.changeFormula(i, j, expression)
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

  def changeCellStylist(index: (Int, Int), stylist: SheetCellStylist) = {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifyStylist(observable.value, stylist)
  }

  def changeCellFormatter(index: (Int, Int), formatter: SheetCellFormatter) = {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifyFormatter(observable.value, formatter)
  }

  def changeCellProperty(index: (Int, Int), styleProperty: String, styleValue: Any) {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifyStyleProperty(observable.value, styleProperty, styleValue)
  }

  def changeCell(index: (Int, Int), expression: String, value: Any) {
    val observable = getCellObservable(index)
    observable.value = SheetCell.newEvaluated(observable.value, expression, value)
  }

  def changeCellExpression(index: (Int, Int), expression: String) {
    _immutableModel.changeFormula(index._1, index._2, expression)
  }

}
