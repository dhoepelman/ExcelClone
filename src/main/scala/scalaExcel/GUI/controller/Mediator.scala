package scalaExcel.GUI.controller

import scalaExcel.GUI.view.ViewManager
import scalaExcel.GUI.modelwrapper.DataModelFactory.DataTable
import scalaExcel.GUI.modelwrapper._
import scalaExcel.model.Styles

object Mediator {

  private val _dataModel = new DataModel
  private var _controller: ViewManager = null

  def initialize() = {
    println("Mediator initializing...")
    _dataModel.populateDataModel(null)
  }

  def tableScrolled(offsets: (Int, Int, Int, Int)) =
    _dataModel.changeTableOffsets(offsets)

  def dataTable: DataTable = _dataModel.dataTable

  def controller = _controller

  def controller_=(manager: ViewManager): Unit = _controller = manager

  def getCellObservable(index: (Int, Int)) =
    _dataModel.getCellObservable((index._1, index._2))

  def getCell(index: (Int, Int)): SheetCell =
    _dataModel.getCell((index._1, index._2))

  def getCellValue(index: (Int, Int)): Any =
    _dataModel.getCellValue((index._1, index._2))

  def setAllCells(values: List[List[String]]): Unit =
    _dataModel.populateDataModel(values)

  def editingCellIndex: (Int, Int) = {
    val cell = _controller.tableView.getEditingCell
    (cell.getRow, cell.getColumn)
  }

  def editingCellChanged(expression: String) = {
    println("Editing cell changed expression to " + expression)
    changeEditorText(expression)
    _dataModel.changeCellExpression(editingCellIndex, expression)
  }

  def changeCellExpression(index: (Int, Int), expression: String) = {
    _dataModel.changeCellExpression(index, expression)
  }

  def changeCellStylist(index: (Int, Int), stylist: SheetCellStylist) =
    _dataModel.changeCellStylist(index, stylist)

  def changeCellProperty(index: (Int, Int), styleProperty: String, styleValue: Any) =
    _dataModel.changeCellProperty(index, styleProperty, styleValue)

  def changeCellFormatter(index: (Int, Int), formatter: SheetCellFormatter) =
    _dataModel.changeCellFormatter(index, formatter)

  def changeEditorText(expression: String) {
    _controller.changeEditorText(expression)
  }

  def columnsReordered(permutations: Map[Int, Int]){
    println("Columns reordered "+permutations)
  }

  def changeCellStyle(index: (Int,Int), style: Styles) = {} // I am dummy

}
