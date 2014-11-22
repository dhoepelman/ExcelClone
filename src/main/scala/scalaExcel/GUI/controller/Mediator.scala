package scalaExcel.GUI.controller

import scalaExcel.GUI.view.ViewManager
import scalaExcel.GUI.modelwrapper._
import scalaExcel.GUI.data.DataManager

object Mediator {

  private val _dataManager = new DataManager
  private var _controller: ViewManager = null
  private val _defaultData = List(List("Cell11", "Cell12"), List("Cell21", "Cell22"))

  def initialize() = {
    println("Mediator initializing...")
    _dataManager.populateDataModel(_defaultData)
  }

  def tableScrolled(offsets: (Int, Int, Int, Int)) =
    _dataManager.tableScrolled(offsets)

  def labeledTable: LabeledDataTable = _dataManager.dataTable

  def controller = _controller

  def controller_=(manager: ViewManager): Unit = _controller = manager

  def getCell(index: (Int, Int)): SheetCell =
  // account for numbered column
    _dataManager.getCell((index._1, index._2 - 1))

  def getCellValue(index: (Int, Int)): Any =
    getCell(index).evaluated

  def setAllCells(values: List[List[String]]): Unit =
    _dataManager.populateDataModel(values)

  def editingCellIndex: (Int, Int) = {
    val cell = _controller.tableView.getEditingCell
    (cell.getRow, cell.getColumn)
  }

  def editingCellChanged(expression: String) = {
    println("Editing cell " + editingCellIndex + "changed expression to " + expression)
    changeEditorText(expression)
    changeCellExpression(editingCellIndex, expression)
  }

  def changeCellExpression(index: (Int, Int), expression: String) =
  // account for numbered column
    _dataManager.changeCellExpression((index._1, index._2 - 1), expression)

  def changeCellStylist(index: (Int, Int), stylist: SheetCellStylist) =
  // account for numbered column
    _dataManager.changeCellStylist((index._1, index._2 - 1), stylist)

  def changeCellProperty(index: (Int, Int), styleProperty: String, styleValue: Any) =
  // account for numbered column
    _dataManager.changeCellProperty((index._1, index._2 - 1), styleProperty, styleValue)

  def changeCellFormatter(index: (Int, Int), formatter: SheetCellFormatter) =
  // account for numbered column
    _dataManager.changeCellFormatter((index._1, index._2 - 1), formatter)

  def changeEditorText(expression: String) =
    _controller.changeEditorText(expression)

  def columnsReordered(permutations: Map[Int, Int]) =
    _dataManager.reorderColumns(permutations)

  def dataChanged(table: LabeledDataTable) = {
    if (_controller != null)
      _controller.buildTableView(table)
  }

  def rebuildTable() =
    _dataManager.refreshData()
}
