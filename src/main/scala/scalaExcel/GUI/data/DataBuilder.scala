package scalaExcel.GUI.data

import scalafx.beans.property.ObjectProperty
import scalaExcel.GUI.modelwrapper.SheetCell
import scalaExcel.GUI.controller.LabeledDataTable._

object DataBuilder {


  private val _defaultDataSize = (10, 10)
  private val _defaultHeaders = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  private val _defaultWidths = List(100.0, 200.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0)

  def buildDataTable(rows: Int, columns: Int, data: Map[(Int, Int), SheetCell]): DataTable = {
    new DataTable() ++=
      List.range(0, rows).map(i => new DataRow() ++=
        List.range(0, columns).map(j => ObjectProperty.apply(data.getOrElse((i, j), SheetCell.newEmpty()))))
  }

  def buildDefaultDataTable: DataTable = {
    buildDataTable(_defaultDataSize._1, _defaultDataSize._2, Map[(Int, Int), SheetCell]())
  }

  def defaultDataWindow = new DataWindow((0, _defaultDataSize._1, 0, _defaultDataSize._2),
    (0, _defaultDataSize._1, 0, _defaultDataSize._2),
    _defaultHeaders,
    _defaultWidths,
    Map[Int, Int](),
    Map[Int, Int]())

  def dataWithIndex(data: List[List[String]]): List[(Int, Int, String)] =
    data.zipWithIndex.map(_ match {
      case (row, i) => row.zipWithIndex.map(_ match {
        case (expression, j) => (i, j, expression)
      })
    }).flatten
}
