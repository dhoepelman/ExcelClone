package scalaExcel.GUI.view

import javafx.scene.{control => jfxsc}
import scalafx.scene.control.TextField
import scalafx.scene.control.cell.TextFieldTableCell
import scalaExcel.GUI.data.{LabeledDataTable, DataCell}
import LabeledDataTable.DataRow
import scalafx.util.StringConverter

class DataCellStringConverter extends StringConverter[DataCell] {
  override def toString(cell: DataCell): String = if (cell == null) "" else cell.toString

  override def fromString(expression: String): DataCell = {
    // return a mock cell instance
    DataCell.newDummy(expression)
  }
}

class DataCellViewDelegate extends jfxsc.cell.TextFieldTableCell[DataRow, DataCell](new DataCellStringConverter) {
  override def startEdit(): Unit = {
    super.startEdit()
    // when cell is being edited, show expression (not value)
    val textField = new TextField(this.getChildren.get(0).asInstanceOf[jfxsc.TextField])
    textField.text = getItem match {
      case null => ""
      case cell => cell.expression
    }
  }
}

class DataCellView extends TextFieldTableCell[DataRow, DataCell](new DataCellViewDelegate) {
  item.onChange {
    (_, _, newCell) =>
      // apply cell customization
      if (newCell != null) {
        style = newCell.styleString
      }
  }
}
