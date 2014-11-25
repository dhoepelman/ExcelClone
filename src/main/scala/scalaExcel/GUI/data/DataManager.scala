package scalaExcel.GUI.data

import scalaExcel.model.{Styles, Model}
import rx.lang.scala.Subject
import rx.lang.scala.subjects.BehaviorSubject
import scalaExcel.GUI.view.ViewManagerObject


object DataManager {

  def initialize() = {
    println("DataManager initializing...")
  }

  private val model = new Model()

  private val _windowActionStream = Subject[WindowActions]()
  private val _windowMutationStream = Subject[WindowMutations]()
  private val _tableMutationStream = BehaviorSubject.apply[TableMutations](RefreshTable())

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
        // in the model, the indexes are swapped
        model.changeFormula(realIndex._2, realIndex._1, expression)
        window
      case ReorderColumns(permutations) =>
        val realPermutations = permutations.map({
          case (c1, c2) => (window.windowToAbsoluteColumn(c1), window.windowToAbsoluteColumn(c2))
        })
        _tableMutationStream.onNext(new UpdateColumnOrder(realPermutations))
        // TODO smth like  model.reorderColumns(realPermutations)
        window
      case SortRows(sortColumn, sortAscending) =>
        val realColumn = window.windowToAbsoluteColumn(sortColumn)
        // TODO smth like model.sortRows(realColumn)
        window
    }).subscribe(_ => Unit)

  model.sheet
    .map(newSheet => {
      newSheet.cells.map({
        case (index, cell) => (
          //in the model, the indexes are swapped
          index.swap,
          cell.f,
          newSheet.valueAt(index._1, index._2).get,
          newSheet.styles.getOrElse(index, Styles.DEFAULT)
        )
      })
    })
    .subscribe(contents => _tableMutationStream.onNext(new UpdateContents(contents)))

  _tableMutationStream.scan(new LabeledDataTable(rebuild = true))((table, action) =>
    action match {
      case UpdateContents(contents) => table.updateContents(contents)
      case UpdateWindow(window) => table.updateWindow(window)
      case UpdateColumnOrder(permutations) => table.updateColumnOrder(permutations)
      case ResizeColumn(columnIndex, width) => table.resizeColumn(columnIndex, width)
      case RefreshTable() => table
    }).subscribe(ViewManagerObject.dataChanged _)

  def tableScrolled(offsets: (Int, Int, Int, Int)) = {
    _windowMutationStream.onNext(new SlideWindowBy(offsets))
  }

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

  def resizeColumn(columnIndex: Int, width: Double) =
    _tableMutationStream.onNext(new ResizeColumn(columnIndex, width))

  def changeCellStylist(index: (Int, Int), stylist: Any) =
    Unit //TODO when styling is ready

  def changeCellProperty(index: (Int, Int), styleProperty: String, styleValue: Any) =
    Unit //TODO when styling is ready

  def changeCellFormatter(index: (Int, Int), formatter: Any) =
    Unit //TODO when styling is ready

}