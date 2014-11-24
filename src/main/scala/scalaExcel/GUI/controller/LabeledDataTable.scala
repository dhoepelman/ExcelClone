package scalaExcel.GUI.controller

import scalafx.collections.ObservableBuffer
import scalafx.beans.property.ObjectProperty
import scalaExcel.GUI.data.DataWindow

class LabeledDataTable(_dataWindow: DataWindow = LabeledDataTable.defaultDataWindow,
                       _cellContents: Iterable[((Int, Int), String, Any)] = List(),
                       _sortColumn: Int = -1,
                       val sortAscending: Boolean = true) {

  val headers = _dataWindow.columnHeaders
  val headerWidths = _dataWindow.columnWidths

  private val translatedContents =
    _cellContents
      .map(content => (_dataWindow.absoluteToWindow(content._1), content._1, content._2, content._3))

  private def contentsToCells(filter: (((Int, Int), (Int, Int), String, Any)) => Boolean) =
    translatedContents
      .filter(filter)
      .foldLeft(Map[(Int, Int), DataCell]())((cells, content) =>
      cells + (content._1 -> DataCell.newEvaluated(content._3, content._4)))

  val data = {
    // transform cell contents contained in window into DataCells
    val _cells = contentsToCells(contents => _dataWindow.isInBounds(contents._1))
    //build data table with the DataCells
    LabeledDataTable.buildDataTable(_dataWindow.rowCount, _dataWindow.columnCount, _cells, _dataWindow)
  }

  def updateContents(contents: Iterable[((Int, Int), String, Any)]) =
    new LabeledDataTable(_dataWindow, contents, _sortColumn, sortAscending)

  def updateWindow(dataWindow: DataWindow) = {
    new LabeledDataTable(dataWindow, _cellContents, _sortColumn, sortAscending)
  }

  def slideWindowBy(offsets: (Int, Int, Int, Int)) = {
    new LabeledDataTable(_dataWindow.slideBy(offsets), _cellContents, _sortColumn, sortAscending)
  }

  def slideWindowTo(bounds: (Int, Int, Int, Int)) = {
    new LabeledDataTable(_dataWindow.slideTo(bounds), _cellContents, _sortColumn, sortAscending)
  }

  def reorderColumns(permutations: Map[Int, Int]) = {
    new LabeledDataTable(_dataWindow.reorderColumns(permutations), _cellContents, _sortColumn, sortAscending)
  }

  def sortRows(sortColumn: Int, sortAscending: Boolean) = {
    val absoluteSortColumn = _dataWindow.windowToAbsoluteColumn(sortColumn)
    new LabeledDataTable(_dataWindow.reorderRows(computeSortOrder(absoluteSortColumn, sortAscending)),
      _cellContents,
      absoluteSortColumn,
      sortAscending)
  }

  def translateIndex(index: (Int, Int)) =
    _dataWindow.windowToAbsolute(index)

  def sortColumn = _dataWindow.absoluteToWindowColumn(_sortColumn)

  def computeSortOrder(sortColumn: Int, sortAscending: Boolean) =
  //    if (sortColumn < 0)
    Map[Int, Int]()

  //    else {
  //      val original = contentsToCells(contents => contents._2._2 == sortColumn).map(cell => cell._2).toList
  //      val nulls = _dataWindow.rowCount - original.length
  //      val sorted = original.sortWith((c1, c2) => if (sortAscending) c1.compare(c2) < 0 else c1.compare(c2) > 0)
  //      val permutations = sorted.view.zipWithIndex.map(pair =>
  //        pair._1.absoluteIndex._1 -> (pair._2 + (if (sortAscending) nulls else 0)))
  //      Map[Int, Int]() ++ permutations
  //    }
}

object LabeledDataTable {
  type DataRow = ObservableBuffer[ObjectProperty[DataCell]]
  type DataTable = ObservableBuffer[DataRow]

  private val _defaultDataSize = (10, 10)
  private val _defaultHeaders = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  private val _defaultWidths = List(100.0, 200.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0)

  def buildDataTable(rows: Int, columns: Int, data: Map[(Int, Int), DataCell], dataWindow: DataWindow): DataTable = {
    new DataTable() ++=
      List.range(0, rows).map(i => new DataRow() ++=
        List.range(0, columns).map(j =>
          ObjectProperty.apply(data.getOrElse((i, j), DataCell.newEmpty()))))
  }

  def defaultDataWindow = new DataWindow((0, _defaultDataSize._1, 0, _defaultDataSize._2),
    (0, _defaultDataSize._1, 0, _defaultDataSize._2),
    _defaultHeaders,
    _defaultWidths,
    Map[Int, Int](),
    Map[Int, Int]())

  def dataWithIndex(data: List[List[String]]): List[(Int, Int, String)] =
    data.zipWithIndex.map({
      case (row, i) => row.zipWithIndex.map({
        case (expression, j) => (i, j, expression)
      })
    }).flatten
}

