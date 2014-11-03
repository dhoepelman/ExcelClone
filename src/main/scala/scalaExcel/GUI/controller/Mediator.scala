package scalaExcel.GUI.controller

import scalaExcel.GUI.model.{SheetCell, ObservableSheetCell, DataModel}
import scalaExcel.GUI.view.ViewManager
import scalaExcel.GUI.model.DataModelFactory.DataTable

object Mediator {

  private var _dataModel: DataModel = null
  private var _controller: ViewManager = null

  def initialize() = {
    println("Mediator initializing...")
    _dataModel = new DataModel()
    _dataModel.populateDataModel(null)
  }

  def dataTable: DataTable = _dataModel.dataTable

  def controller = _controller

  def controller_=(manager: ViewManager): Unit = _controller = manager

  def getCellObservable(index: (Int, Int)): ObservableSheetCell =
    _dataModel.getCellObservable(index._1, index._2)

  def getCell(index: (Int, Int)): SheetCell =
    _dataModel.getCell(index._1, index._2)

  def getCellValue(index: (Int, Int)): Any =
    _dataModel.getCellValue(index._1, index._2)

  def getEditingCellIndex: (Int, Int) = {
    val cell = _controller.getTableView.getEditingCell
    (cell.getRow, cell.getColumn)
  }

  def composeEditingCell(expr: String): SheetCell = {
    val index = getEditingCellIndex
    val cell = getCell(index)
    println("Editing cell " + index + " changed expression to " + expr)
    changeEditorText(expr)
    SheetCell.modifyExpr(index, cell, expr)
  }

  def changeCellExpr(index: (Int, Int), expr: String) =
    _dataModel.changeCellExpr(index, expr)

  def changeCellStylist(index: (Int, Int), stylist: Any => String) =
    _dataModel.changeCellStylist(index, stylist)

  def changeCellFormatter(index: (Int, Int), formatter: Any => String) =
    _dataModel.changeCellFormatter(index, formatter)

  def changeEditorText(expr: String) {
    _controller.changeEditorText(expr)
  }

}
