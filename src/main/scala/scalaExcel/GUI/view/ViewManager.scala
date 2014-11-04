package scalaExcel.GUI.view

import java.net.URL
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.paint.Color
import javafx.scene.{input => jfxsi}
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import rx.lang.scala._

import scala.collection.mutable
import scala.util._
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

  def initialize(url: URL, rb: java.util.ResourceBundle) {

    backgroundColorPicker = new ColorPicker(backgroundColorPickerDelegate);
    tableContainer = new AnchorPane(tableContainerDelegate)
    formulaEditor = new TextField(formulaEditorDelegate)
    testButton = new Button(testButtonDelegate)

    table = TableViewBuilder.build(null, null, Mediator.dataTable)
    AnchorPane.setAnchors(table, 0, 0, 0, 0)
    tableContainer.content = List(table)

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
                        .map(x => new {val position = x._2; val formula = x._1}) // For better readability
                        .distinctUntilChanged(x => x.formula)
                        .subscribe(x => Mediator.changeCellExpr((x.position._1, x.position._2), x.formula))

    // Changes on backgroundColorPicker are reflected on the selected cell
    backgroundColorStream.map(colorToWeb)
                          .combineLatest(selectionStream)
                          .map(x => new { val position = x._2; val colour = x._1}) // For better readability
                          .distinctUntilChanged(x => x.colour)
                          .subscribe(x => Mediator.changeCellStylist(x.position, _=>"-fx-background-color: " + x.colour + ";"))


    // Update formula editor when selection changes
    // TODO change all the tools to fit the cell
    selectedCellStream.subscribe(x => {
      changeEditorText(x.exprString)
    })

    selectedCellStream.map(x => x.stylist.apply())
                      .map(x => fieldsFromCss(x).getOrElse("-fx-background-color", "#FFFFFF"))
                      .map(x => Color.web(x))
                      .subscribe(x => changeBackgroundColorPicker(x))
//                      .subscribe(x => println(x))

  }

  def changeEditorText(text: String) = formulaEditor.setText(text)
  def changeBackgroundColorPicker(color: Color) = backgroundColorPicker.setValue(color)

  def tableView: TableView[DataRow] = table

  def colorToWeb(c : Color): String =
      "#%02X%02X%02X".format(
      (c.getRed() * 255).asInstanceOf[Int],
      (c.getGreen() * 255).asInstanceOf[Int],
      (c.getBlue() * 255).asInstanceOf[Int])

  def fieldsFromCss(css : String) : (scala.collection.mutable.Map[String, String]) = {
    val bodyRe = """([^:;{}]+:[^:;{}]+;?)""".r

    val map = new mutable.HashMap[String, String]
    bodyRe.findAllIn(css)
          .map(pair => pair.split(":"))
          .map(tokens => (tokens(0).trim -> tokens(1).trim.replace(";","")))
          .foreach(x => map += x) // TODO there's probably a more FP way

    return map
  }

}
