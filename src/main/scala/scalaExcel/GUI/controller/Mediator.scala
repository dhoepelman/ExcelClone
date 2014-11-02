package scalaExcel.GUI.controller

import scalaExcel.GUI.model.{SheetCell, ObservableSheetCell, DataModel}
import scalaExcel.GUI.view.SheetBuilder

object Mediator {

  private val dataModel = new DataModel()
  dataModel.populateDataModel(null)
  private val tableView = SheetBuilder.build(null, null, dataModel.getDataTable)

  def initialize() = {
    println("Mediator initializing")
  }

  def isInitialized = {
    tableView != null
  }

  def getCellObservable(index: (Int, Int)): ObservableSheetCell =
    dataModel.getCellObservable(index._1, index._2)

  def getCell(index: (Int, Int)): SheetCell =
    dataModel.getCell(index._1, index._2)

  def getCellValue(index: (Int, Int)): Any =
    dataModel.getCellValue(index._1, index._2)

  def getEditingCellindex: (Int, Int) = {
    val cell = tableView.getEditingCell
    (cell.getRow, cell.getColumn)
  }

  def getTableView = tableView

  def composeEditingCell(expr: String): SheetCell = {
    val index = getEditingCellindex
    val cell = getCell(index)
    println("Editing cell " + index + " changed expression to " + expr)
    SheetCell.modifyExpr(index, cell, expr)
  }

  def changeCellExpr(index: (Int, Int), expr: String) =
    dataModel.changeCellExpr(index, expr)

  def changeCellStylist(index: (Int, Int), stylist: Any => String) =
    dataModel.changeCellStylist(index, stylist)

  def changeCellFormatter(index: (Int, Int), formatter: Any => String) =
    dataModel.changeCellFormatter(index, formatter)
}
