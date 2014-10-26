package scalaExcel.GUI.app

import scalafx.scene.control._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.cell.TextFieldTableCell
import scalafx.beans.property.ObjectProperty

object SheetBuilder {

  type RowBuffer = ObservableBuffer[ObjectProperty[SheetCell]]
  type ColumnBuffer = ObservableBuffer[javafx.scene.control.TableColumn[RowBuffer, SheetCell]]


  def getColumnList(headers: List[String], widths: List[Int]): ColumnBuffer =
    getColumnListReversed(headers.reverse, widths.reverse)

  def getColumnListReversed(headers: List[String], widths: List[Int]): ColumnBuffer = {
    if (headers == List())
      new ColumnBuffer()
    else
      getColumnListReversed(headers.tail, widths.tail) += new TableColumn[RowBuffer, SheetCell] {
        text = headers.head
        cellValueFactory = {
          _.value.get(headers.length - 1)
        }
        cellFactory = {
          column =>
            new TextFieldTableCell[RowBuffer, SheetCell](new SheetCellStringConverter(column)) {
              item.onChange {
                (_, _, newCell) =>
                  style = if (newCell == null) "" else newCell.stylist(null)
                //                  graphic = new TextField() {
                //                    text = if (newCell == null) "" else newCell.objectString.value
                //                  }
              }
            }
        }
        prefWidth = widths.head
      }
  }

  private def getPlainRow(data: List[Any]): RowBuffer = {
    if (data == List())
      new RowBuffer()
    else {
      val modifiedStylist = (_: Any) => "-fx-background-color: yellow;"
      val cell = if (data.head.toString.endsWith("1")) new SheetCell(data.head, null, modifiedStylist) else new SheetCell(data.head, null, null)
      new RowBuffer() += new ObjectProperty(cell, "cell", cell) ++= getPlainRow(data.tail)
    }
  }

  def getPlainData(data: List[List[Any]]): ObservableBuffer[RowBuffer] = {
    if (data == List())
      new ObservableBuffer[RowBuffer]()
    else
      new ObservableBuffer[RowBuffer]() += getPlainRow(data.head) ++= getPlainData(data.tail)
  }

  def build(columnHeaders: List[String], columnWidths: List[Int], rows: ObservableBuffer[RowBuffer]) = {
    // Add value change listeners
    rows.view.zipWithIndex.foreach {
      case (row, i) => row.view.zipWithIndex.foreach {
        case (cell, j) => cell.onChange((a, oldValue, newValue) => {
          println(i + " " + j + " changed from " + oldValue + " to " + newValue)
        })
      }
    }
    new TableView[RowBuffer](rows) {
      editable = true
      columns ++= getColumnList(columnHeaders, columnWidths)
    }
  }
}
