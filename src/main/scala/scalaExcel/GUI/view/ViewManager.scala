package scalaExcel.GUI.view

import scalafx.Includes._
import java.net.URL
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}
import javafx.stage.FileChooser.ExtensionFilter
import javafx.stage.{FileChooser, Stage}
import javafx.{event => jfxe, fxml => jfxf}
import rx.lang.scala._

import scalafx.scene.layout.AnchorPane
import scalaExcel.GUI.controller.Mediator
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalaExcel.GUI.modelwrapper.DataModelFactory.DataRow
import scalafx.scene.paint.Color
import scalaExcel.GUI.util.CSSHelper
import scalaExcel.GUI.util.Filer

import scala.language.reflectiveCalls


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

  @jfxf.FXML private var menuLoadDelegate: javafx.scene.control.MenuItem = _
  @jfxf.FXML private var menuSaveDelegate: javafx.scene.control.MenuItem = _
  private var menuLoad: scalafx.scene.control.MenuItem = _
  private var menuSave: scalafx.scene.control.MenuItem = _

  @jfxf.FXML
  private var testButtonDelegate: jfxsc.Button = _
  private var testButton: Button = _

  @jfxf.FXML
  private def makeResizable(event: jfxe.ActionEvent) {
    //make region resizable
  }

  val fileChooser = new javafx.stage.FileChooser
  fileChooser.getExtensionFilters.add(new ExtensionFilter("Comma separated values", "*.csv"))

  def initialize(url: URL, rb: java.util.ResourceBundle) {

    //
    // Initialization of GUI object handles
    //

    backgroundColorPicker = new ColorPicker(backgroundColorPickerDelegate)
    fontColorPicker = new ColorPicker(fontColorPickerDelegate)
    tableContainer = new AnchorPane(tableContainerDelegate)
    formulaEditor = new TextField(formulaEditorDelegate)
    testButton = new Button(testButtonDelegate)
    menuLoad = new MenuItem(menuLoadDelegate)
    menuSave = new MenuItem(menuSaveDelegate)

    // initialize and add the table
    val stage = formulaEditor.delegate.getScene.asInstanceOf[Stage]

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
    //Save requests
    val saveStream = Observable.create[String](o => new Subscription {
      menuSave.onAction = handle {
        o.onNext("temp.csv")
      }
    })
    // Load requests
    val loadStream = Observable.create[String](o => new Subscription {
      menuLoad.onAction = handle {
        o.onNext("temp.csv")
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
        if (x.size == 1) x.head._2.style
        else ""
      })
      .subscribe(x => {
        changeBackgroundColorPicker(CSSHelper.colorFromCssOrElse(x, "-fx-background-color", Color.White))
        changeFontColorPicker(CSSHelper.colorFromCssOrElse(x, "-fx-text-fill", Color.Black))
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
    backgroundColorStream.map(("-fx-background-color", _))
      .merge(fontColorStream.map(("-fx-text-fill", _)))
      .combineLatest(selectedCellStream)
      .map(x => new {
        val cells = x._2
        val definition = x._1
      }) // For better readability
      .distinctUntilChanged(_.definition)
      .subscribe(x => x.cells.foreach(cell =>
        Mediator.changeCellProperty(cell._1, x.definition._1, x.definition._2)))


    // Load - Save
    saveStream.map(x => {
                fileChooser.setTitle("Save destination")
                fileChooser
              })
              .map(chooser => chooser.showSaveDialog(stage))
              .filter(_!=null)
              .subscribe(file => Filer.saveCSV(file, Mediator.dataTable))

    loadStream.map(x => {
                fileChooser.setTitle("Open file")
                fileChooser
              })
              .map(chooser => chooser.showOpenDialog(stage))
              .filter(_!=null)
              .map(file => Filer.loadCSV(file))
              .subscribe(data => Mediator.setAllCells(data))
  }

  def changeEditorText(text: String) = formulaEditor.text = text

  def changeBackgroundColorPicker(color: Color) = backgroundColorPicker.value = color

  def changeFontColorPicker(color: Color) = fontColorPicker.value = color

  def tableView: TableView[DataRow] = table

}
