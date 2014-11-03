package scalaExcel.GUI.view

import java.net.URL
import java.util
import javafx.scene.{input => jfxsi}
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import rx.lang.scala._

import scalafx.scene.layout.AnchorPane
import scalaExcel.GUI.controller.Mediator
import scalaExcel.GUI.model.SheetCell
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
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
  private var testButtonDelegate: jfxsc.Button = _
  private var testButton: Button = _

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

    assert(tableContainerDelegate != null)
    assert(formulaEditorDelegate != null)
    assert(testButtonDelegate != null)
    tableContainer = new AnchorPane(tableContainerDelegate)
    formulaEditor = new TextField(formulaEditorDelegate)
    testButton = new Button(testButtonDelegate)
    assert(tableContainer != null)
    assert(formulaEditor != null)
    assert(testButton != null)

    table = TableViewBuilder.build(null, null, Mediator.dataTable)
    AnchorPane.setAnchors(table, 0, 0, 0, 0)
    tableContainer.content = List(table)

    Mediator.changeCellStylist((0, 0), SheetCell.makeYellow)
    Mediator.changeCellExpr((0, 0), "new")
    Mediator.changeCellStylist((0, 1), SheetCell.makeGreen)

    val selectionModel = table.getSelectionModel
    selectionModel.setCellSelectionEnabled(true)
    selectionModel.setSelectionMode(SelectionMode.MULTIPLE)

    // Test button click stream
    val obs = Observable.create[Unit](o => new Subscription {
      testButton.setOnMouseClicked(new jfxe.EventHandler[jfxsi.MouseEvent] {
        def handle(mouseEvent: jfxsi.MouseEvent): Unit = {
          o.onNext()
        }
      })
    })
    obs.subscribe(x => println("Test button clicked"))

    // Create cell selection stream
    val selectedCells = new ObservableBuffer(selectionModel.getSelectedCells)
    val selectionStream = Observable.create[(Int, Int)](o => new Subscription {
      selectedCells.onChange((source, changes) => {
        o.onNext((source.map(x => (x.getRow, x.getColumn)).head))
      })
    })

    // Update formula editor when selection changes
    selectionStream.subscribe(x => changeEditorText(Mediator.getCell(x._1, x._2).exprString))
  }

  def changeEditorText(text: String) = formulaEditor.setText(text)

  def getTableView: TableView[DataRow] = table

}
