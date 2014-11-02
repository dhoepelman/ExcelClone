package scalaExcel.GUI.model

import scalaExcel.GUI.model.DataModelFactory.DataTable

class DataModel() {
  private val defaultData = List(List("Cell11", "Cell12"), List("Cell21", "Cell22"))

  private val dataTable = DataModelFactory.buildDefaultDataTable

  private def defaultCellBuilder(location: (Int, Int), expr: String): SheetCell =
    if (expr.toString.endsWith("1")) {
      SheetCell.newYellow(location, expr)
    } else {
      SheetCell.newNormal(location, expr)
    }

  private def populateDataTable(raw: DataTable,
                                data: List[List[String]],
                                cellBuilder: ((Int, Int), String) => SheetCell) =
    raw.view.zip(data).view.zipWithIndex.foreach {
      case ((rawRow, dataRow), i) => rawRow.view.zip(dataRow).view.zipWithIndex.foreach {
        case ((cell, expr), j) => cell.value = SheetCell.newNormal((i, j), expr)
      }
    }

  private def populateDataTable(raw: DataTable,
                                data: List[List[String]]): Unit =
    populateDataTable(raw, data, defaultCellBuilder)

  private def populateDataTableWithDefault(raw: DataTable): Unit =
    populateDataTable(raw, defaultData, defaultCellBuilder)

  def getCellObservable(row: Int, column: Int): ObservableCell = dataTable.get(row).get(column)

  def getCell(row: Int, column: Int): SheetCell = getCellObservable(row, column).value

  def getCellValue(row: Int, column: Int): Any = getCell(row, column).evaluated

  def getDataTable = dataTable

  def populateDataModel(data: List[List[String]]) =
    if (data == null)
      populateDataTableWithDefault(dataTable)
    else
      populateDataTable(dataTable, data)

}
