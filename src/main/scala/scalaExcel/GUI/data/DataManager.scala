package scalaExcel.GUI.data

import scalaExcel.model.Model
import scalaExcel.GUI.controller.{Mediator, LabeledDataTable}
import rx.lang.scala.Subject


class DataManager {

  private val _immutableModel = new Model()

  private val _windowActionStream = Subject[WindowActions]()
  private val _windowMutationStream = Subject[WindowMutations]()
  private val _tableMutationStream = Subject[TableMutations]()

  private val windowStream = _windowMutationStream.scan(LabeledDataTable.defaultDataWindow)((window, action) =>
    action match {
      case SlideWindowBy(offsets) => window.slideBy(offsets)
      case SlideWindowTo(bounds) => window.slideTo(bounds)
      case RefreshWindow() => window
    })
  windowStream.subscribe(window => _tableMutationStream.onNext(new UpdateWindow(window)))
  windowStream.subscribe(window => _windowActionStream.onNext(new NewWindow(window)))

  _windowActionStream.scan(LabeledDataTable.defaultDataWindow)((window, action) =>
    action match {
      case NewWindow(newWindow) => newWindow
      case ChangeCellExpression(index, expression) =>
        val realIndex = window.windowToAbsolute(index)
        _immutableModel.changeFormula(realIndex._1, realIndex._2, expression)
        window
      case ReorderColumns(permutations) =>
        // TODO smth like  _immutableModel.reorderColumns(permutations)
        window
      case SortRows(sortColumn, sortAscending) =>
        // TODO smth like _immutableModel.reorderRows(permutations)
        window
    }).subscribe(_ => Unit)

  _immutableModel.sheet.map(
    newSheet => newSheet.cells.map(
      cell => (cell._1, cell._2.f, newSheet.valueAt(cell._1._1, cell._1._2).get)))
    .subscribe(contents => _tableMutationStream.onNext(new UpdateContents(contents)))

  _tableMutationStream.scan(new LabeledDataTable())((table, action) =>
    action match {
      case UpdateContents(contents) => table.updateContents(contents)
      case UpdateWindow(window) => table.updateWindow(window)
      case RefreshTable() => table
    }).subscribe(Mediator.dataChanged _)

  def tableScrolled(offsets: (Int, Int, Int, Int)) = {
    _windowMutationStream.onNext(new SlideWindowBy(offsets))
  }

  def populateDataModel(data: List[List[String]]) =
    LabeledDataTable.dataWithIndex(data).foreach(cell => _immutableModel.changeFormula(cell._1, cell._2, cell._3))

  def changeCellExpression(index: (Int, Int), expression: String) = {
    println("Changing " + index + " to " + expression)
    _windowActionStream.onNext(new ChangeCellExpression(index, expression))
  }

  def reorderColumns(permutations: Map[Int, Int]) =
    _windowActionStream.onNext(new ReorderColumns(permutations))

  def sortRows(sortColumn: Int, sortAscending: Boolean) =
    _windowActionStream.onNext(new SortRows(sortColumn, sortAscending))

  def refreshData() =
    _tableMutationStream.onNext(new RefreshTable())

  def changeCellStylist(index: (Int, Int), stylist: Any) =
    Unit //TODO when styling is ready

  def changeCellProperty(index: (Int, Int), styleProperty: String, styleValue: Any) =
    Unit //TODO when styling is ready

  def changeCellFormatter(index: (Int, Int), formatter: Any) =
    Unit //TODO when styling is ready

}
