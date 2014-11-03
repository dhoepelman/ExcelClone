package scalaExcel.GUI.view

import java.net.URL
import java.util
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.paint.Color
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
  private var backgroundColorPickerDelegate: javafx.scene.control.ColorPicker = _
  private var backgroundColorPicker: scalafx.scene.control.ColorPicker = _

  @jfxf.FXML
  private var testButtonDelegate: jfxsc.Button = _
  private var testButton: Button = _

  @jfxf.FXML
  private def makeResizable(event: jfxe.ActionEvent) {
    //make region resizable
  }

//  @jfxf.FXML
//  private def handleEditorChange(event: jfxe.ActionEvent) {
//    val editorValue = formulaEditor.getText
//    val selectedCells = new ObservableBuffer(table.getSelectionModel.getSelectedCells)
//
//  }

  def initialize(url: URL, rb: util.ResourceBundle) {

    backgroundColorPicker = new ColorPicker(backgroundColorPickerDelegate);
    tableContainer = new AnchorPane(tableContainerDelegate)
    formulaEditor = new TextField(formulaEditorDelegate)
    testButton = new Button(testButtonDelegate)

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

    // Create cell selection stream (indices)
    val selectedCells = new ObservableBuffer(selectionModel.getSelectedCells)
    val selectionStream = Observable.create[(Int, Int)](o => new Subscription {
      selectedCells.onChange((source, changes) => {
        o.onNext((source.map(x => (x.getRow, x.getColumn)).head))
      })
    }).distinctUntilChanged
    // Selected cell stream
    val selectedCellStream = selectionStream.map(x => Mediator.getCell(x._1, x._2));

    val backgroundColorStream = Observable.create[Color](o => new Subscription {
      backgroundColorPicker.delegate.setOnAction(new EventHandler[ActionEvent] {
        override def handle(event: ActionEvent): Unit = {
          o.onNext(backgroundColorPicker.value.value)
        }
      })
    })

    val formulaEditorStream = Observable.create[String](o => new Subscription {
      formulaEditor.delegate.setOnAction(new EventHandler[ActionEvent] {
        override def handle(event: ActionEvent): Unit = {
          o.onNext(formulaEditor.getText)
        }
      })
    })

    // Changes on formula editor are reflected on the selected cell
    formulaEditorStream.combineLatest(selectionStream)
                        .distinctUntilChanged(x => x._1)
                        .subscribe(x => Mediator.changeCellExpr((x._2._1, x._2._2), x._1))


    // Update formula editor when selection changes
    // TODO change all the tools to fit the cell
    selectedCellStream.subscribe(x => changeEditorText(x.exprString))



  }

  def changeEditorText(text: String) = formulaEditor.setText(text)

  def getTableView: TableView[DataRow] = table

}
