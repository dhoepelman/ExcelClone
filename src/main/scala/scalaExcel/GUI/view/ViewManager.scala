package scalaExcel.GUI.view

import scalafx.Includes._
import java.net.URL
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import rx.lang.scala._

import scalafx.scene.layout.AnchorPane
import scalaExcel.GUI.controller.Mediator
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalaExcel.GUI.model.DataModelFactory.DataRow
import scalafx.scene.paint.Color


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
    // Initialization of GUI object handles
    //

    backgroundColorPicker = new ColorPicker(backgroundColorPickerDelegate)
    fontColorPicker = new ColorPicker(fontColorPickerDelegate)
    tableContainer = new AnchorPane(tableContainerDelegate)
    formulaEditor = new TextField(formulaEditorDelegate)
    testButton = new Button(testButtonDelegate)

    // initialize and add the table
    table = TableViewBuilder.build(null, null, Mediator.dataTable)
    val selectionModel = table.getSelectionModel
    selectionModel.setCellSelectionEnabled(true)
    selectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    AnchorPane.setAnchors(table, 0, 0, 0, 0)
    tableContainer.content = List(table)

    //
    // Create streams
    //

    // Create cell selection stream (indices)
    val selectedCells = new ObservableBuffer(selectionModel.getSelectedCells)
    val selectionStream = List() +: Observable.create[Iterable[(Int, Int)]](o => new Subscription {
      selectedCells.onChange((source, changes) => {
        o.onNext(source.map(x => (x.getRow, x.getColumn)))
      })
    })
    // Create cell selection stream (SheetCell)
    val selectedCellStream = selectionStream.map(_.map(x => (x, Mediator.getCell(x))))

    // The user input on the background colour
    val backgroundColorStream = Observable.create[Color](o => new Subscription {
      backgroundColorPicker.onAction = handle {
        o.onNext(backgroundColorPicker.value.value)
      }
    })
    // The user input on the font colour
    val fontColorStream = Observable.create[Color](o => new Subscription {
      fontColorPicker.onAction = handle {
        o.onNext(fontColorPicker.value.value)
      }
    })
    // The user input on the formula
    val formulaEditorStream = Observable.create[String](o => new Subscription {
      formulaEditor.onAction = handle {
        o.onNext(formulaEditor.getText)
      }
    })

    //
    // Behavior
    //

    // Update toolbar when selection changes
    // Update the formula editor
    selectedCellStream.subscribe(x => {
      if (x.size == 1) changeEditorText(x.head._2.expression)
      else changeEditorText("")
    })
    // Update color pickers when selection changes
    selectedCellStream.map(x => {
      if (x.size == 1) fieldsFromCss(x.head._2.stylist.apply())
      else fieldsFromCss("")
    })
      .subscribe(fields => {
      changeBackgroundColorPicker(Color.web(fields.getOrElse("-fx-background-color", "#FFFFFF")))
      changeFontColorPicker(Color.web(fields.getOrElse("-fx-text-fill", "#000000")))
    })

    // Changes on formula editor are pushed to the selected cell
    formulaEditorStream.combineLatest(selectionStream)
      .map(x => new {
      val positions = x._2
      val formula = x._1
    }) // For better readability
      .distinctUntilChanged(_.formula)
      .subscribe(x => x.positions.foreach(Mediator.changeCellExpression(_, x.formula)))

    // Changes on the ColorPickers are pushed to the model
    backgroundColorStream.map("-fx-background-color: " + colorToWeb(_))
      .merge(fontColorStream.map("-fx-text-fill: " + colorToWeb(_)))
      .combineLatest(selectedCellStream)
      .map(x => new {
      val cells = x._2
      val definition = x._1
    }) // For better readability
      .distinctUntilChanged(_.definition)
      .subscribe(x => x.cells.foreach(cell => Mediator.changeCellStylist(cell._1, _ => setCssField(cell._2.stylist(), x.definition))))
  }


  def changeEditorText(text: String) = formulaEditor.text = text

  def changeBackgroundColorPicker(color: Color) = backgroundColorPicker.value = color

  def changeFontColorPicker(color: Color) = fontColorPicker.value = color

  def tableView: TableView[DataRow] = table


  /*******************************************
      Utility functions
    *******************************************/


  private def colorToWeb(c: Color): String =
    "#%02X%02X%02X".format(
      (c.red * 255).asInstanceOf[Int],
      (c.green * 255).asInstanceOf[Int],
      (c.blue * 255).asInstanceOf[Int])

  private def fieldsFromCss(css: String): (Map[String, String]) = {
    val bodyRe = """([^:;{}]+:[^:;{}]+;?)""".r
    bodyRe.findAllIn(css)
      .map(pair => pair.split(":"))
      .map(tokens => tokens(0).trim -> tokens(1).trim.replace(";", ""))
      .foldLeft(Map[String, String]())((m, x) => m + x)
  }

  private def fieldsToCss(fields: Map[String, String]): String =
    fields.foldLeft("")((s, x) => s + x._1 + ": " + x._2 + "; ")


  private def setCssField(css: String, field: String, value: String): String =
    fieldsToCss(fieldsFromCss(css) + (field -> value))


  private def setCssField(css: String, property: String): String = {
    val tokens = property.split(":").map(x => x.trim)
    setCssField(css, tokens(0), tokens(1))
  }

}
