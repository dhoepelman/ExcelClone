package scalaExcel.GUI.modelwrapper

import scalafx.collections.ObservableBuffer
import scalafx.beans.property.ObjectProperty

object DataModelFactory {

  type DataRow = ObservableBuffer[ObjectProperty[SheetCell]]
  type DataTable = ObservableBuffer[DataRow]

  private val defaultSize = (10, 10)

  private def buildDataTable(rows: Int,
                             columns: Int): DataTable = {
    new DataTable() ++=
      List.range(0, rows).map(i => new DataRow() ++=
        List.range(0, columns).map(j => ObjectProperty.apply(SheetCell.newEmpty())))
  }

  def buildDefaultDataTable: DataTable = {
    buildDataTable(defaultSize._1, defaultSize._2)
  }
}
