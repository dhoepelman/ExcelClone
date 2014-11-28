package scalaExcel.GUI.view

import scalaExcel.model.Model
import scalaExcel.GUI.data.LabeledDataTable

class ViewManagerObject(
    val model: Model,
    val controller: ViewManager
  ) {

  println("ViewManager initializing...")

  /** Observer when the user somehow changes the cell */
  val onCellEdit = controller.onCellEdit
  /** Events when the background color of a cell is selected */
  val onBackgroundChange = controller.onBackgroundChange
  /** When the front color of a cell is selected */
  val onColorChange = controller.onColorChange

  def dataChanged(labeledTable: LabeledDataTable) =
    controller.buildTableView(labeledTable, model)

}
