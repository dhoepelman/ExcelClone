package scalaExcel.GUI.view

import scalaExcel.GUI.data.LabeledDataTable

object ViewManager {
  private var _controller: FXController = null

  def initialize(fxController: FXController) =  {
    println("ViewManager initializing...")
    _controller = fxController
  }

  def dataChanged(labeledTable: LabeledDataTable) = _controller.buildTableView(labeledTable)
  def changeEditorText(text: String) = _controller.changeEditorText(text)
}
