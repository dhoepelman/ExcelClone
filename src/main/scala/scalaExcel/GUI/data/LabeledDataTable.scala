package scalaExcel.GUI.data

import scalafx.collections.ObservableBuffer
import scalafx.beans.property.ObjectProperty
import scalaExcel.model.Styles
import scalaExcel.util.{ColumnTranslator, DefaultProperties}

class LabeledDataTable(_dataWindow: DataWindow = LabeledDataTable.defaultDataWindow,
                       _allHeaderWidths: List[Double] = LabeledDataTable.defaultHeaderWidths,
                       _cellContents: Iterable[((Int, Int), String, Any, Styles)] = List(),
                       _sortColumn: Int = -1,
                       val sortAscending: Boolean = true,
                       val rebuild: Boolean) {

  def headers = LabeledDataTable.getHeaders(_dataWindow.visibleBounds)

  def headerWidths = _allHeaderWidths.drop(_dataWindow.visibleBounds._3).take(_dataWindow.columnCount)

  def sortColumn = _dataWindow.absoluteToWindowColumn(_sortColumn)

  private val translatedContents =
    _cellContents
      .map(content => (_dataWindow.absoluteToWindow(content._1), content._2, content._3, content._4))

  private def contentsToCells(filter: (((Int, Int), String, Any, Styles)) => Boolean) =
    translatedContents
      .filter(filter)
      .foldLeft(Map[(Int, Int), DataCell]())({
      case (cells, (index, formula, value, style)) =>
        cells + (index -> DataCell.newEvaluated(formula, value, style))
    })

  val data = {
    // transform cell contents contained in window into DataCells
    val _cells = contentsToCells(contents => _dataWindow.isInBounds(contents._1))
    //build data table with the DataCells
    LabeledDataTable.buildDataTable(_dataWindow.rowCount,
      _dataWindow.columnCount,
      _cells,
      _dataWindow)
  }

  def updateContents(contents: Iterable[((Int, Int), String, Any, Styles)]) =
    new LabeledDataTable(_dataWindow,
      _allHeaderWidths,
      contents,
      _sortColumn,
      sortAscending,
      false)

  def updateWindow(dataWindow: DataWindow) = {
    new LabeledDataTable(dataWindow,
      _allHeaderWidths,
      _cellContents,
      _sortColumn,
      sortAscending,
      rebuild = true)
  }

  def updateColumnOrder(permutations: Map[Int, Int]) = {
    val reversePermutations = permutations map (_.swap)
    val newWidths = List.range(0, _allHeaderWidths.length)
      .map(i => _allHeaderWidths(reversePermutations.getOrElse(i, i)))
    new LabeledDataTable(_dataWindow,
      newWidths,
      _cellContents,
      _sortColumn,
      sortAscending,
      rebuild = true)
  }

}

object LabeledDataTable {
  type DataRow = ObservableBuffer[ObjectProperty[DataCell]]
  type DataTable = ObservableBuffer[DataRow]

  def buildDataTable(rows: Int,
                     columns: Int,
                     data: Map[(Int, Int), DataCell],
                     dataWindow: DataWindow): DataTable = {
    new DataTable() ++=
      List.range(0, rows + 1).map(i => new DataRow() ++=
        List.range(0, columns + 1).map(j =>
          ObjectProperty.apply(data.getOrElse((i, j), DataCell.newEmpty()))))
  }

  def getHeaders(bounds: (Int, Int, Int, Int)) =
    List.range(bounds._3, bounds._4) map ColumnTranslator.numToCol

  val defaultDataWindow = new DataWindow(
    (0, DefaultProperties.GRID_SIZE._1, 0, DefaultProperties.GRID_SIZE._2),
    (0, DefaultProperties.GRID_SIZE._1, 0, DefaultProperties.GRID_SIZE._2))

  val defaultHeaderWidths =
    List.fill(defaultDataWindow.columnCount)(DefaultProperties.COLUMN_WIDTH)

  def dataWithIndex(data: List[List[String]]): List[(Int, Int, String)] =
    data.zipWithIndex.map({
      case (row, i) => row.zipWithIndex.map({
        case (expression, j) => (i, j, expression)
      })
    }).flatten
}

