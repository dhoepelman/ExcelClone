package scalaExcel.GUI.controller

import scalaExcel.GUI.model.{SheetCell, ObservableCell, DataModel}
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

  def getCellObservable(location: (Int, Int)): ObservableCell =
    dataModel.getCellObservable(location._1, location._2)

  def getCell(location: (Int, Int)): SheetCell =
    dataModel.getCell(location._1, location._2)

  def getCellValue(location: (Int, Int)): Any =
    dataModel.getCellValue(location._1, location._2)

  def getEditingCellLocation: (Int, Int) = {
    val cell = tableView.getEditingCell
    (cell.getRow, cell.getColumn)
  }

  def cellEvaluated(location: (Int, Int), value: Any) = {
    println("Cell " + location + " evaluated to " + value)
    val observable = getCellObservable(location._1, location._2)
    if (observable.value != null)
      observable.value = SheetCell.markEvaluated(location, observable.value, value)
  }

  def getTableView = tableView

  def changeEditingCellExpr(expr: String): SheetCell = {
    val location = getEditingCellLocation
    val cell = getCell(location._1, location._2)
    println("Editing cell " + location + " changed expression to " + expr)
    SheetCell.modifyExpr(location, cell, expr)
  }

  def changeCellExpr(location: (Int, Int), expr: String) = {
    val observable = getCellObservable(location._1, location._2)
    println("Cell " + location + " changed expression to " + expr)
    observable.value = SheetCell.modifyExpr(location, observable.value, expr)
  }

}
