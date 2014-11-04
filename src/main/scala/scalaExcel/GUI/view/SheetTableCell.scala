package scalaExcel.GUI.view

import javafx.scene.{control=> jfxsc}
import scalaExcel.GUI.model.DataModelFactory._
import scalaExcel.GUI.model.SheetCell
import scalaExcel.GUI.model.SheetCellStringConverter.SheetCellStringConverter
import scalafx.scene.control.TextField
import scalaExcel.GUI.controller.Mediator

class SheetTableCell() extends jfxsc.cell.TextFieldTableCell[DataRow, SheetCell](new SheetCellStringConverter) {
    override def startEdit(): Unit = {
      super.startEdit()
      val textField = new TextField(this.getChildren.get(0).asInstanceOf[jfxsc.TextField])
      textField.text = getItem.expr
      Mediator.changeEditorText(getItem.expr)
    }

    override def commitEdit(p1: SheetCell): Unit = {
      //super.commitEdit()
      super.cancelEdit()
    }
}
