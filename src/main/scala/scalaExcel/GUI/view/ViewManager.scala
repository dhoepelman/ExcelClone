package scalaExcel.GUI.view

import java.net.URL
import java.util
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import scalafx.scene.layout.AnchorPane
import scalaExcel.GUI.controller.Mediator
import scalaExcel.GUI.model.SheetCell
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{TableView, TextField, SelectionMode}
import scalaExcel.GUI.model.DataModelFactory.DataRow


class ViewManager extends jfxf.Initializable {

  private var table: TableView[DataRow] = _

  @jfxf.FXML
  private var redRegion: jfxsl.Region = _

  @jfxf.FXML
  private var tableContainerDelegate: jfxsl.AnchorPane = _
  private var tableContainer: AnchorPane = _

  @jfxf.FXML
  private var formulaEditorDelegate: javafx.scene.control.TextField = _
  private var formulaEditor: TextField = _

  @jfxf.FXML
  private def makeResizable(event: jfxe.ActionEvent) {
    //make region resizable
  }

  @jfxf.FXML
  private def handleEditorChange(event: jfxe.ActionEvent) {
    val editorValue = formulaEditor.getText
    val selectedCells = new ObservableBuffer(table.getSelectionModel.getSelectedCells)
    selectedCells.map(x => Mediator.changeCellExpr((x.getRow, x.getColumn), editorValue))
  }

  def initialize(url: URL, rb: util.ResourceBundle) {
    tableContainer = new AnchorPane(tableContainerDelegate)
    formulaEditor = new TextField(formulaEditorDelegate)

    table = SheetBuilder.build(null, null, Mediator.getDataTable)
    AnchorPane.setAnchors(table, 0, 0, 0, 0)
    tableContainer.content = List(table)

    Mediator.changeCellStylist((0, 0), SheetCell.makeYellow)
    Mediator.changeCellExpr((0, 0), "new")
    Mediator.changeCellStylist((0, 1), SheetCell.makeGreen)

    val selectionModel = table.getSelectionModel
    selectionModel.setCellSelectionEnabled(true)
    selectionModel.setSelectionMode(SelectionMode.MULTIPLE)

    // Display selected cell onto the formula editor
    val selectedCells = new ObservableBuffer(selectionModel.getSelectedCells)
    selectedCells.onChange(
      (source, changes) => {
        source.take(1).map(x =>
          changeEditorText(Mediator.getCell(x.getRow, x.getColumn).exprString))
      }
    )
  }

  def changeEditorText(text: String) = formulaEditor.setText(text)

  def getTableView: TableView[DataRow] = table

}
