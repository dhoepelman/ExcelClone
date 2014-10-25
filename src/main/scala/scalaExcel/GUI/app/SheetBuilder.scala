package scalaExcel.GUI.app

import scalafx.scene.control._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TableColumn._

object SheetBuilder {

  type RowBuffer = ObservableBuffer[SheetCell]
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
          _.value.get(headers.length - 1).asProperty
        }
        // Render the property value when it changes,
        // including initial assignment
        cellFactory = {
          column =>
            new TableCell[RowBuffer, SheetCell] {
              item.onChange {
                (_, _, newCell) =>
                  style = if (newCell == null) "" else newCell.stylist.value(null)
                  graphic = new TextField() {
                    text = if (newCell == null) "" else newCell.objectString.value
                  }
              }
            }
        }
        prefWidth = widths.head
      }
  }

  private def getPlainRow(data: List[Any]): RowBuffer = {
    if (data == List())
      new RowBuffer()
    else
      new RowBuffer() += new SheetCell(data.head, null, null) ++= getPlainRow(data.tail)
  }

  def getPlainData(data: List[List[Any]]): ObservableBuffer[RowBuffer] = {
    if (data == List())
      new ObservableBuffer[RowBuffer]()
    else
      new ObservableBuffer[RowBuffer]() += getPlainRow(data.head) ++= getPlainData(data.tail)
  }

  def build(columnHeaders: List[String], columnWidths: List[Int], rows: ObservableBuffer[RowBuffer]) =
    new TableView[RowBuffer](rows) {
      columns ++= getColumnList(columnHeaders, columnWidths)
    }
}
