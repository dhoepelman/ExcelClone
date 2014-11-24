package scalaExcel.GUI.controller

import scalaExcel.GUI.view.ViewManager
import scalaExcel.GUI.data.DataManager

object Mediator {

  private val _dataManager = new DataManager
  private var _controller: ViewManager = null
  private val _defaultData = List(List("Cell11", "Cell12"), List("Cell21", "Cell22"))

  def initialize(controller: ViewManager) = {
    println("Mediator initializing...")
    _controller = controller
    _dataManager.populateDataModel(_defaultData)
  }

  def tableScrolled(offsets: (Int, Int, Int, Int)) =
    _dataManager.tableScrolled(offsets)

  def setAllCells(values: List[List[String]]): Unit =
    _dataManager.populateDataModel(values)

  def changeCellExpression(index: (Int, Int), expression: String) =
    _dataManager.changeCellExpression(index, expression)

  def changeCellStylist(index: (Int, Int), stylist: Any) =
    _dataManager.changeCellStylist(index, stylist)

  def changeCellProperty(index: (Int, Int), styleProperty: String, styleValue: Any) =
    _dataManager.changeCellProperty(index, styleProperty, styleValue)

  def changeCellFormatter(index: (Int, Int), formatter: Any) =
    _dataManager.changeCellFormatter(index, formatter)

  def changeEditorText(expression: String) =
    _controller.changeEditorText(expression)

  def columnsReordered(permutations: Map[Int, Int]) =
    _dataManager.reorderColumns(permutations)

  def rowsSorted(sortColumn: Int, sortAscending: Boolean) =
    _dataManager.sortRows(sortColumn, sortAscending)

  def dataChanged(table: LabeledDataTable) = {
    if (_controller != null && table.data != null)
      _controller.buildTableView(table)
  }

  def rebuildTable() =
    _dataManager.refreshData()
}
