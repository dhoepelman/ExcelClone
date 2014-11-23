package scalaExcel.GUI.data

import rx.lang.scala.subjects.BehaviorSubject
import scalaExcel.model.Model
import scalaExcel.GUI.controller.{Mediator, LabeledDataTable}
import scalaExcel.GUI.modelwrapper.{SheetCellFormatter, SheetCellStylist}


class DataManager {

  private val _immutableModel = new Model()

  private val _dataChanges = BehaviorSubject[DataChanges](new RefreshData())

  _immutableModel.sheet.map(
    newSheet => newSheet.cells.map(
      cell => (cell._1, cell._2.f, newSheet.valueAt(cell._1._1, cell._1._2).get)))
    .subscribe(contents => _dataChanges.onNext(new UpdateContents(contents)))

  _dataChanges.scan(new LabeledDataTable(DataBuilder.defaultDataWindow, List()))((table, action) => action match {
    case ModifyFormulaAt(index, formula) =>
      val realIndex = table.translateIndex(index)
      println("changing formula of "+realIndex+" with "+formula)
      _immutableModel.changeFormula(realIndex._1, realIndex._2, formula)
      table.flushData()
    case UpdateContents(contents) => table.updateContents(contents)
    case SlideWindowBy(offsets) => table.slideWindowBy(offsets)
    case ReorderColumns(permutations) => table.reorderColumns(permutations)
    case RefreshData() => table
  }).subscribe(Mediator.dataChanged _)

  def tableScrolled(offsets: (Int, Int, Int, Int)) = {
    _dataChanges.onNext(new SlideWindowBy(offsets))
  }

  def populateDataModel(data: List[List[String]]) =
    DataBuilder.dataWithIndex(data).foreach(cell => _immutableModel.changeFormula(cell._1, cell._2, cell._3))

  def changeCellExpression(index: (Int, Int), expression: String) = {
    _dataChanges.onNext(new ModifyFormulaAt(index, expression))
  }

  def reorderColumns(permutations: Map[Int, Int]) =
    _dataChanges.onNext(new ReorderColumns(permutations))

  def refreshData() =
    _dataChanges.onNext(new RefreshData())

  def changeCellStylist(index: (Int, Int), stylist: SheetCellStylist) =
    Unit //TODO when styling is ready

  def changeCellProperty(index: (Int, Int), styleProperty: String, styleValue: Any) =
    Unit //TODO when styling is ready

  def changeCellFormatter(index: (Int, Int), formatter: SheetCellFormatter) =
    Unit //TODO when styling is ready

}
