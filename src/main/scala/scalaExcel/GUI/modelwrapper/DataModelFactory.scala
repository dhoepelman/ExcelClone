package scalaExcel.GUI.modelwrapper

import scalafx.collections.ObservableBuffer

object DataModelFactory {

  type DataRow = ObservableBuffer[ObservableSheetCell]
  type DataTable = ObservableBuffer[DataRow]

  private val defaultSize = (10, 10)

  private def buildDataTable(rows: Int,
                             columns: Int): DataTable = {
    new DataTable() ++=
      List.range(0, rows).map(i => new DataRow() ++=
        List.range(0, columns).map(j => new ObservableSheetCell(i, j, null)))
  }

  def buildDefaultDataTable: DataTable = {
    buildDataTable(defaultSize._1, defaultSize._2)
  }

  def buildCustomDataTable(size: (Int, Int), data: List[List[String]]): DataTable = {
    buildDataTable(defaultSize._1, defaultSize._2)
  }
}
