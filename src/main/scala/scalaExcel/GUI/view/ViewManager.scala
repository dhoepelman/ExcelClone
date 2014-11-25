package scalaExcel.GUI.view

import scalaExcel.model.Styles
import scalafx.Includes._
import java.net.URL
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}
import javafx.stage.FileChooser.ExtensionFilter
import javafx.{event => jfxe, fxml => jfxf}
import rx.lang.scala._

import scalafx.scene.layout.AnchorPane
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.paint.Color
import scalaExcel.GUI.util.CSSHelper
import scalaExcel.GUI.util.Filer

import scala.language.reflectiveCalls
import scalaExcel.GUI.data.{DataManager, DataCell, LabeledDataTable}
import LabeledDataTable.DataRow
import scalafx.beans.property.ObjectProperty

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

  def getObservableAt(index: (Int, Int)) =
  // account for numbered column
    if (index._2 < 0) ObjectProperty.apply(DataCell.newEmpty())
    else table.items.getValue.get(index._1).get(index._2)

  def buildTableView(labeledTable: LabeledDataTable): Unit = {
    println("Building table")
    // initialize and add the table

    table = TableViewBuilder.build(labeledTable)
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
    }).map(selection => selection.map{case (x: Int, y: Int) => (x, y-1)})
      .doOnEach(selection => println("Selection: " + selection))
    // Create cell selection stream (DataCell), accounting for numbered column
    val selectedCellStream = selectionStream.map(_.map(x => (x, getObservableAt(x).value)))
    val selectionStylesStream = selectedCellStream.map(_.map(x => (x._1, x._2.styles)))

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
        if (x.size == 1) x.head._2.styleString
        else ""
      })
      .subscribe(x => {
        changeBackgroundColorPicker(CSSHelper.colorFromCssOrElse(x, "-fx-background-color", Styles.DEFAULT.background))
        changeFontColorPicker(CSSHelper.colorFromCssOrElse(x, "-fx-text-fill", Styles.DEFAULT.color))
    })

    // Changes on formula editor are pushed to the selected cell
    formulaEditorStream.combineLatest(selectedCellStream)
      .map(x => new {
        val cells = x._2
        val formula = x._1
      }) // For better readability
      .distinctUntilChanged(_.formula)
      .subscribe(x => x.cells.foreach(cell =>
        if (cell._1._2 > 0)
          DataManager.changeCellExpression((cell._1._1, cell._1._2), x.formula)
      ))

    // Changes on the ColorPickers are pushed to the model
    val styleChangerBackgroundStream = backgroundColorStream
      .map(colour => setBackground(colour)_)
    val styleChangerTextFillStream = fontColorStream
      .map(colour => setTextFill(colour)_)
    styleChangerBackgroundStream
      .merge(styleChangerTextFillStream)
      .labelAlways(selectionStylesStream)
      .map(c => c.label.map(cellStyle => (cellStyle._1, c.value(cellStyle._2))))
      .subscribe(_.foreach(newStyle => DataManager.changeCellStylist(newStyle._1, newStyle._2)))


    // Load - Save
    saveStream.map(x => {
        fileChooser.setTitle("Save destination")
        fileChooser
      })
      .map(chooser => chooser.showSaveDialog(tableContainer.scene.window.getValue))
      .filter(_ != null)
      .subscribe(file => Filer.saveCSV(file, table.items.getValue))

    loadStream.map(x => {
        fileChooser.setTitle("Open file")
        fileChooser
      })
      .map(chooser => chooser.showOpenDialog(tableContainer.scene.window.getValue))
      .filter(_ != null)
      .map(file => Filer.loadCSV(file))
      .subscribe(data => DataManager.populateDataModel(data))
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
    menuLoad = new MenuItem(menuLoadDelegate)
    menuSave = new MenuItem(menuSaveDelegate)

  }


  implicit class ExtendRx[T](ob: Observable[T]) {
    def labelAlways[L](la: Observable[L]) =
      ob.combineLatest(la)
        .distinctUntilChanged(_._1)
        .map(c => new {val value = c._1; val label = c._2})
  }

  def changeEditorText(text: String) = formulaEditor.text = text

  def changeBackgroundColorPicker(color: Color) = backgroundColorPicker.value = color

  def changeFontColorPicker(color: Color) = fontColorPicker.value = color

  def tableView: TableView[DataRow] = table


  def setBackground(colour: Color)(style: Styles): Styles = style.setBackground(colour)
  def setTextFill(colour: Color)(style: Styles): Styles = style.setColor(colour)
}
