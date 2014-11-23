package scalaExcel.GUI.modelwrapper

import scalaExcel.model.Model
import scalaExcel.GUI.modelwrapper.DataModelFactory.DataTable

class DataModel {
  private val defaultData = List(List("Cell11", "Cell12"), List("Cell21", "Cell22"))

  private val _dataTable = DataModelFactory.buildDefaultDataTable

  private val _immutableModel = new Model()

  private var _tableOffsets = (0, _dataTable.size, 0, _dataTable.get(0).size)

  _immutableModel.sheet
    .map(
      newSheet => newSheet.cells.map(
        cell => (cell._1, cell._2.f, newSheet.valueAt(cell._1._1, cell._1._2).get)))
    .subscribe(
      sheetContent => sheetContent.foreach(
        x => safeChangeCell((x._1._1 - _tableOffsets._1, x._1._2 - _tableOffsets._3), x._2, x._3)))

  def dataTable = _dataTable

  def changeTableOffsets(newOffsets: (Int, Int, Int, Int)) = {
    _tableOffsets = newOffsets
    clearDataTable
    _immutableModel.refresh()
  }

  def clearDataTable = {
    _dataTable.map(row => row.map(cell => cell.value = SheetCell.newEmpty((-1, -1))))
  }

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

  def getCellObservable(index: (Int, Int)) = _dataTable.get(index._1).get(index._2)

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

  def changeCellProperty(index: (Int, Int), styleProperty: String, styleValue: Any) = {
    val observable = getCellObservable(index)
    observable.value = SheetCell.modifyStyleProperty(observable.value, styleProperty, styleValue)
  }

  def safeChangeCell(index: (Int, Int), expression: String, value: Any) =
    if (index._1 >= 0 && index._1 < _dataTable.size && index._2 >= 0 && index._2 < _dataTable.get(0).size)
      changeCell(index, expression, value)

  def changeCell(index: (Int, Int), expression: String, value: Any) = {
    val observable = getCellObservable(index)
    println("Cell " + index + " changed from " + observable.value + " to expr=" + expression + " val=" + value)
    observable.value = SheetCell.newEvaluated((-1, -1), expression, value)
  }

  def changeCellExpression(index: (Int, Int), expression: String) = {
    _immutableModel.changeFormula(index._1 + _tableOffsets._1, index._2 + _tableOffsets._3, expression)
  }

}
