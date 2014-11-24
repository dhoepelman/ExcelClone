package scalaExcel.GUI.view

import javafx.scene.{control => jfxsc}
import scalafx.scene.control.TextField
import scalaExcel.GUI.controller.{DataCell, Mediator}
import scalafx.scene.control.cell.TextFieldTableCell
import scalaExcel.GUI.controller.LabeledDataTable.DataRow
import scalafx.util.StringConverter


class DataCellStringConverter(column: DataCellColumn) extends StringConverter[DataCell] {
  override def toString(cell: DataCell): String = if (cell == null) "" else cell.toString

  override def fromString(expression: String): DataCell = {
    //TODO move to tableview
    val index = column.getTableView.getEditingCell
    // account for numbered column
    Mediator.changeCellExpression((index.getRow, index.getColumn - 1), expression)
    Mediator.changeEditorText(expression)
    // return a mock cell instance
    DataCell.newEmpty()
  }
}

class DataCellViewDelegate(column: DataCellColumn) extends jfxsc.cell.TextFieldTableCell[DataRow, DataCell](new DataCellStringConverter(column)) {
  override def startEdit(): Unit = {
    super.startEdit()
    // when cell is being edited, show expression (not value)
    val textField = new TextField(this.getChildren.get(0).asInstanceOf[jfxsc.TextField])
    textField.text = getItem match {
      case null => ""
      case cell => cell.expression
    }
    Mediator.changeEditorText(textField.getText)
  }

  override def commitEdit(p1: DataCell): Unit = {
    // do not commit changes directly (the data model will update)
    super.cancelEdit()
  }
}

class DataCellView(column: DataCellColumn) extends TextFieldTableCell[DataRow, DataCell](new DataCellViewDelegate(column)) {
  item.onChange {
    (_, _, newCell) =>
    // apply cell customization
      if (newCell != null) style = newCell.style
  }
}
