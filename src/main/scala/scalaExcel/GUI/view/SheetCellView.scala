package scalaExcel.GUI.view

import javafx.scene.{control=> jfxsc}
import scalaExcel.GUI.modelwrapper.SheetCell
import SheetCellStringConverter.SheetCellStringConverter
import scalafx.scene.control.TextField
import scalaExcel.GUI.controller.Mediator
import scalafx.scene.control.cell.TextFieldTableCell
import scalaExcel.GUI.controller.LabeledDataTable.DataRow

class SheetCellViewDelegate(column: SheetCellColumn) extends jfxsc.cell.TextFieldTableCell[DataRow, SheetCell](new SheetCellStringConverter(column)) {
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

  override def commitEdit(p1: SheetCell): Unit = {
    // do not commit changes directly (the data model will update)
    super.cancelEdit()
  }
}

class SheetCellView(column: SheetCellColumn) extends TextFieldTableCell[DataRow, SheetCell](new SheetCellViewDelegate(column)) {
  item.onChange {
    (_, _, newCell) =>
      // apply cell customization
      if (newCell != null) style = newCell.style
  }
}
