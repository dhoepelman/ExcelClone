package scalaExcel.GUI.controller

import scalafx.collections.ObservableBuffer
import scalafx.beans.property.ObjectProperty
import scalaExcel.GUI.modelwrapper.{SheetCellFormatter, SheetCellStylist, SheetCell}
import scalaExcel.GUI.data.{DataBuilder, DataWindow}

class LabeledDataTable(_dataWindow: DataWindow, _cellContents: Iterable[((Int, Int), String, Any)]) {

  val headers = _dataWindow.columnHeaders
  val headerWidths = _dataWindow.columnWidths
  val data = {
    //transform Cells into SheetCells
    val cells = _cellContents.foldLeft(Map[(Int, Int), SheetCell]())((cells, content) => {
      val index = _dataWindow.absoluteToWindow(content._1)
      cells + (index -> SheetCell.newEvaluated(null, content._2, content._3))
    })
    //build data table with the SheetCells
    DataBuilder.buildDataTable(_dataWindow.rowCount, _dataWindow.columnCount, cells)
  }

  def updateContents(contents: Iterable[((Int, Int), String, Any)]) =
    new LabeledDataTable(_dataWindow, contents)

  def slideWindowBy(offsets: (Int, Int, Int, Int)) = {
    new LabeledDataTable(_dataWindow.slideBy(offsets), _cellContents)
   }

  def slideWindowTo(bounds: (Int, Int, Int, Int)) = {
    new LabeledDataTable(_dataWindow.slideTo(bounds), _cellContents)
  }

  def reorderColumns(permutations: Map[Int, Int]) = {
   new LabeledDataTable(_dataWindow.reorderColumns(permutations), _cellContents)
  }

  def translateIndex(index: (Int, Int)) =
    _dataWindow.windowToAbsolute(index)

  def getCellObservable(index: (Int, Int)) = data.get(index._1).get(index._2)

  def getCell(index: (Int, Int)): SheetCell = getCellObservable(index).value

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
}

object LabeledDataTable {
  type DataRow = ObservableBuffer[ObjectProperty[SheetCell]]
  type DataTable = ObservableBuffer[DataRow]
}

