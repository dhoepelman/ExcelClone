package scalaExcel.GUI.view

import javafx.scene.{control => jfxsc}
import scalaExcel.GUI.model.DataModelFactory._
import scalaExcel.GUI.model.SheetCell
import SheetCellStringConverter.SheetCellStringConverter
import scalafx.scene.control.TextField
import scalaExcel.GUI.controller.Mediator
import scalafx.scene.control.cell.TextFieldTableCell
import scalaExcel.GUI.util.{CircularEvaluation, ErroneousEvaluation}

class SheetCellViewDelegate() extends jfxsc.cell.TextFieldTableCell[DataRow, SheetCell](new SheetCellStringConverter) {
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

class SheetCellView() extends TextFieldTableCell[DataRow, SheetCell](new SheetCellViewDelegate()) {
  item.onChange {
    (_, _, newCell) =>
    // apply cell customization
      style = {
        if (newCell == null)
          ""
        else newCell.evaluated match {
          case x: ErroneousEvaluation => SheetCell.makeError(null)
          case x: CircularEvaluation => SheetCell.makeError(null)
          case _ => newCell.stylist(null)
        }
      }
  }
}
