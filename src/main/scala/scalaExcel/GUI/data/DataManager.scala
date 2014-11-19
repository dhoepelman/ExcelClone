package scalaExcel.GUI.data

import rx.lang.scala.subjects.BehaviorSubject
import scalaExcel.model.Model
import scalaExcel.GUI.controller.{Mediator, LabeledDataTable}
import rx.lang.scala.Subject
import scalaExcel.GUI.modelwrapper.{SheetCellFormatter, SheetCellStylist, SheetCell}


class DataManager {

  private val _immutableModel = new Model()

  private val windowChanges = Subject[DataWindowChanges]()

  val dataWindow = windowChanges.scan(DataBuilder.defaultDataWindow)((window, action) => action match {
    case MoveWindowBy(offsets) => window.slide(offsets)
    case ReorderColumns(permutations) => window.reorderColumns(permutations)
    case RefreshWindow() => window
  })
  dataWindow.subscribe(window => dataChanges.onNext(new WindowChanged(window)))

  private val dataChanges = BehaviorSubject[DataChanges](new RefreshData())

  _immutableModel.sheet.map(
    newSheet => newSheet.cells.map(
      cell => (cell._1, cell._2.f, newSheet.valueAt(cell._1._1, cell._1._2).get)))
    .subscribe(contents => dataChanges.onNext(new ContentsChanged(contents)))

  private val tableGenerator = dataChanges.scan(new LabeledDataTable(DataBuilder.defaultDataWindow, List()))((table, action) => action match {
    case ContentsChanged(contents) => table.changeContents(contents)
    case WindowChanged(window) => table.changeWindow(window)
    case RefreshData() => table
  })

  def tableScrolled(offsets: (Int, Int, Int, Int)) = {
    windowChanges.onNext(new MoveWindowBy(offsets))
  }

  def populateDataModel(data: List[List[String]]) =
    DataBuilder.dataWithIndex(data).foreach(cell => _immutableModel.changeFormula(cell._1, cell._2, cell._3))

  def changeCellExpression(index: (Int, Int), expression: String) = {
    val absoluteIndex = _dataTable.translateIndex(index)
    _immutableModel.changeFormula(absoluteIndex._1, absoluteIndex._2, expression)
  }

  def reorderColumns(permutations: Map[Int, Int]) =
  windowChanges.onNext(new ReorderColumns(permutations))

  //TODO modify these
  private var _dataTable: LabeledDataTable = null
  tableGenerator.subscribe(table => {Mediator.dataChanged(table); _dataTable = table})

  def dataTable = _dataTable

  def getCell(index: (Int, Int)): SheetCell =
    _dataTable.getCell(index)

  def changeCellStylist(index: (Int, Int), stylist: SheetCellStylist) =
    _dataTable.changeCellStylist(index, stylist)

  def changeCellProperty(index: (Int, Int), styleProperty: String, styleValue: Any) =
    _dataTable.changeCellProperty(index, styleProperty, styleValue)

  def changeCellFormatter(index: (Int, Int), formatter: SheetCellFormatter) =
    _dataTable.changeCellFormatter(index, formatter)

}
