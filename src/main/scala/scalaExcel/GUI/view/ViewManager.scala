package scalaExcel.GUI.view

import java.net.URL
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.paint.Color
import javafx.scene.{input => jfxsi}
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import rx.lang.scala._

import scala.annotation.meta.field
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
  private var fontColorPickerDelegate: javafx.scene.control.ColorPicker = _
  private var fontColorPicker: scalafx.scene.control.ColorPicker = _

  @jfxf.FXML
  private var testButtonDelegate: jfxsc.Button = _
  private var testButton: Button = _

  @jfxf.FXML
  private def makeResizable(event: jfxe.ActionEvent) {
    //make region resizable
  }

  def initialize(url: URL, rb: java.util.ResourceBundle) {

    //
    // Initialisation of GUI object handles
    //

    backgroundColorPicker = new ColorPicker(backgroundColorPickerDelegate)
    fontColorPicker = new ColorPicker(fontColorPickerDelegate)
    tableContainer = new AnchorPane(tableContainerDelegate)
    formulaEditor = new TextField(formulaEditorDelegate)
    testButton = new Button(testButtonDelegate)

    table = TableViewBuilder.build(null, null, Mediator.dataTable)
    AnchorPane.setAnchors(table, 0, 0, 0, 0)
    tableContainer.content = List(table)

    val selectionModel = table.getSelectionModel
    selectionModel.setCellSelectionEnabled(true)
    selectionModel.setSelectionMode(SelectionMode.MULTIPLE)


    //
    // Create streams
    //

    // Create cell selection stream (indices)
    val selectedCells = new ObservableBuffer(selectionModel.getSelectedCells)
    val selectionStream = List() +: Observable.create[Iterable[(Int, Int)]](o => new Subscription {
      selectedCells.onChange((source, changes) => {
        o.onNext((source.map(x => (x.getRow, x.getColumn))))
      })
    })
    // Create cell selection stream (SheetCell)
    val selectedCellStream = selectionStream
      .filter(x => x!=null)
      .map(it => it.map(x => Mediator.getCell(x._1, x._2)));

    // The user input on the background colour
    val backgroundColorStream = Observable.create[Color](o => new Subscription {
      backgroundColorPicker.delegate.setOnAction(new EventHandler[ActionEvent] {
        override def handle(event: ActionEvent): Unit = {
          o.onNext(backgroundColorPicker.value.value)
        }
      })
    })
    // The user input on the font colour
    val fontColorStream = Observable.create[Color](o => new Subscription {
      fontColorPicker.delegate.setOnAction(new EventHandler[ActionEvent] {
        override def handle(event: ActionEvent): Unit = {
          o.onNext(fontColorPicker.value.value)
        }
      })
    })
    // The user input on the formula
    val formulaEditorStream = Observable.create[String](o => new Subscription {
      formulaEditor.delegate.setOnAction(new EventHandler[ActionEvent] {
        override def handle(event: ActionEvent): Unit = {
          o.onNext(formulaEditor.getText)
        }
      })
    })


    //
    // Behaviour
    //

    // Update toolbar when selection changes
    // Update the formula editor
    selectedCellStream.subscribe(x => {
      if (x.size == 1) changeEditorText(x.head.expression)
      else changeEditorText("")
    })
    // Update color pickers when selection changes
    selectedCellStream.map(x => {
      if (x.size == 1) fieldsFromCss(x.head.stylist.apply())
      else fieldsFromCss("")
    })
      .subscribe(fields => {
      changeBackgroundColorPicker(Color.web(fields.getOrElse("-fx-background-color", "#FFFFFF")))
      changeFontColorPicker      (Color.web(fields.getOrElse("-fx-text-fill",        "#000000")))
    })

    // Changes on formula editor are pushed to the selected cell
    formulaEditorStream.combineLatest(selectionStream)
      .map(x => new {val positions = x._2; val formula = x._1}) // For better readability
      .distinctUntilChanged(x => x.formula)
      .filter(x => x.positions != null)
      .subscribe(x => x.positions.foreach(position => Mediator.changeCellExpression((position._1, position._2), x.formula)))

    // Changes on the ColorPickers are pushed to the model
    backgroundColorStream.map(x => "-fx-background-color: " + colorToWeb(x))
      .merge(fontColorStream.map(x => "-fx-text-fill: " + colorToWeb(x)))
      .combineLatest(selectionStream)
      .filter(x => x._2 != null)
      .distinctUntilChanged(x => x._1)
      .flatMap(x => Observable.from(x._2.map(p => (x._1, p))))
      .map(x => (x._2, x._1, Mediator.getCell(x._2._1, x._2._2).stylist()))
      .map(x => (x._1, setCssField(x._3, x._2)))
      .subscribe(x => Mediator.changeCellStylist(x._1, _=>x._2))
  }




  def changeEditorText(text: String) = formulaEditor.setText(text)
  def changeBackgroundColorPicker(color: Color) = backgroundColorPicker.setValue(color)
  def changeFontColorPicker(color: Color) = fontColorPicker.setValue(color)

  def tableView: TableView[DataRow] = table



  /*******************************************
      Utility functions
   *******************************************/


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
      .map(tokens => tokens(0).trim -> tokens(1).trim.replace(";",""))
      .foreach(x => map += x) // TODO there's probably a more FP way
    return map
  }

  def fieldsToCss(fields : scala.collection.mutable.Map[String, String]) : String = {
    val sb = new StringBuilder;
    fields.foreach(x => sb ++= x._1 + ": " + x._2 + "; ")
    return "" + sb//"{" + sb + "}"
  }

  def setCssField(css: String, field: String, value: String) : String = {
    val fields = fieldsFromCss(css)
    fields(field) = value
    fieldsToCss(fields)
  }

  def setCssField(css: String, property: String) : String = {
    val tokens = property.split(":").map(x => x.trim)
    setCssField(css, tokens(0), tokens(1))
  }

  // Stand out
  def printmy(any: Any) = println(" - " + any)

}
