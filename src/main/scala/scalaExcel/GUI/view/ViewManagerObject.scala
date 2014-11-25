package scalaExcel.GUI.view

import scalaExcel.GUI.data.LabeledDataTable

object ViewManagerObject {

  private var _controller: ViewManager = null

  def initialize(fxController: ViewManager) = {
    println("ViewManager initializing...")
    _controller = fxController
  }

  def dataChanged(labeledTable: LabeledDataTable) = _controller.buildTableView(labeledTable)
  def changeEditorText(text: String) = _controller.changeEditorText(text)

}
