package scalaExcel.GUI.data

import scalaExcel.model.Model
import rx.lang.scala.Subject


class DataManager(val model: Model) {

  println("DataManager initializing...")

  /**
   * Streams
   */

  val modelMutations = Subject[ModelMutations]()
  val windowMutations = Subject[WindowMutations]()
  val tableMutations = Subject[TableMutations]()

  // this stream will emit the latest version of the window applied on the model
  private val window = windowMutations.scan(LabeledDataTable.defaultDataWindow)((window, action) =>
    action match {
      case SlideWindowBy(offsets) => window.slideBy(offsets)
      case SlideWindowTo(bounds) => window.slideTo(bounds)
      case RefreshWindow() => window
    })

  // this stream can be used to manipulate the model through the current window;
  // it emits the current window on each manipulation
  modelMutations.scan(LabeledDataTable.defaultDataWindow)((window, action) =>
    action match {
      case UpdateModelWindow(newWindow) => newWindow
      case ChangeFormula(index, expression) =>
        val realIndex = window.windowToAbsolute(index)
        model.changeFormula(realIndex, expression)
        window
      case ReorderColumns(permutations) =>
        val realPermutations = permutations.map({
          case (c1, c2) => (window.windowToAbsoluteColumn(c1), window.windowToAbsoluteColumn(c2))
        })
        tableMutations.onNext(new UpdateColumnOrder(realPermutations))
        // TODO smth like  model.reorderColumns(realPermutations)
        window
      case SortColumn(column, ascending) =>
        val realColumn = window.windowToAbsoluteColumn(column)
        model.sortColumn(realColumn, ascending)
        window
      case ChangeBackground(index, color) =>
        val realIndex = window.windowToAbsolute(index)
        model.changeBackground(realIndex, color)
        window
      case ChangeColor(index, color) =>
        val realIndex = window.windowToAbsolute(index)
        model.changeColor(realIndex, color)
        window
    })
    // this stream needs a dummy subscriber in order to keep emitting
    .subscribe(_ => Unit)


  /**
   * Subscriptions
   */

  // the table updates its contents on each modification of the sheet
  model.sheet.subscribe(sheet => tableMutations.onNext(new UpdateContents(sheet)))
  // the table uses the window to update its contents
  window.subscribe(window => tableMutations.onNext(new UpdateWindow(window)))
  // the model manipulation stream uses the window to translate indexes when posting to the model
  window.subscribe(window => modelMutations.onNext(new UpdateModelWindow(window)))

  /**
   * An Rx stream of LabeledDataTables, that should be drawn on the screen
   */
  val labeledDataTable = tableMutations.scan(new LabeledDataTable(rebuild = true))((table, action) =>
    action match {
      case UpdateContents(newSheet) => table.updateContents(newSheet)
      case UpdateWindow(newWindow) => table.updateWindow(newWindow)
      case UpdateColumnOrder(permutations) => table.updateColumnOrder(permutations)
      case ResizeColumn(columnIndex, width) => table.resizeColumn(columnIndex, width)
      case RefreshTable() => table
    })
}
