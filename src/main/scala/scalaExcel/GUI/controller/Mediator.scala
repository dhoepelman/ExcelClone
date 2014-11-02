package scalaExcel.GUI.controller

import scalaExcel.GUI.model.{SheetCell, ObservableSheetCell, DataModel}
import scalaExcel.GUI.view.ViewManager
import scalaExcel.GUI.model.DataModelFactory.DataTable

object Mediator {

  private var dataModel: DataModel = null
  private var controller: ViewManager = null

  def initialize() = {
    println("Mediator initializing...")
    dataModel = new DataModel()
    dataModel.populateDataModel(null)
  }

  def getDataTable: DataTable = dataModel.dataTable

  def registerController(manager: ViewManager) =
    controller = manager

  def getCellObservable(index: (Int, Int)): ObservableSheetCell =
    dataModel.getCellObservable(index._1, index._2)

  def getCell(index: (Int, Int)): SheetCell =
    dataModel.getCell(index._1, index._2)

  def getCellValue(index: (Int, Int)): Any =
    dataModel.getCellValue(index._1, index._2)

  def getEditingCellIndex: (Int, Int) = {
    val cell = controller.getTableView.getEditingCell
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
    dataModel.changeCellExpr(index, expr)

  def changeCellStylist(index: (Int, Int), stylist: Any => String) =
    dataModel.changeCellStylist(index, stylist)

  def changeCellFormatter(index: (Int, Int), formatter: Any => String) =
    dataModel.changeCellFormatter(index, formatter)

  def changeEditorText(expr: String) {
    controller.changeEditorText(expr)
  }

}
