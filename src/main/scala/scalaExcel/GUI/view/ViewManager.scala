package scalaExcel.GUI.view

import scalaExcel.model.Styles
import scalaExcel.model.Model
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

  private var streamTable: StreamingTable = _
  private var table: TableView[DataRow] = _

  @jfxf.FXML private var tableContainerDelegate: jfxsl.AnchorPane = _
  private var tableContainer: AnchorPane = _

  @jfxf.FXML private var formulaEditorDelegate: javafx.scene.control.TextField = _
  private var formulaEditor: TextField = _
  private var formulaEditorStream: Observable[String] = _

  @jfxf.FXML private var backgroundColorPickerDelegate: javafx.scene.control.ColorPicker = _
  private var backgroundColorPicker: scalafx.scene.control.ColorPicker = _
  private var backgroundColorStream: Observable[Color] = _

  @jfxf.FXML private var fontColorPickerDelegate: javafx.scene.control.ColorPicker = _
  private var fontColorPicker: scalafx.scene.control.ColorPicker = _
  private var fontColorStream: Observable[Color] = _

  @jfxf.FXML private var menuLoadDelegate: javafx.scene.control.MenuItem = _
  private var menuLoad: scalafx.scene.control.MenuItem = _
  private var loadStream: Observable[String] = _

  @jfxf.FXML private var menuSaveDelegate: javafx.scene.control.MenuItem = _
  private var menuSave: scalafx.scene.control.MenuItem = _
  private var saveStream: Observable[String] = _

  @jfxf.FXML
  private var testButtonDelegate: jfxsc.Button = _
  private var testButton: Button = _

  @jfxf.FXML
  private def makeResizable(event: jfxe.ActionEvent) {
    //make region resizable
  }

  val fileChooser = new javafx.stage.FileChooser
  fileChooser.getExtensionFilters.add(new ExtensionFilter("Comma separated values", "*.csv"))

  // Exposed observers, so we can gather those events and put
  // them into the model

  val onCellEdit = Subject[((Int, Int), String)]()
  val onBackgroundChange = Subject[((Int, Int), Color)]()
  val onColorChange = Subject[((Int, Int), Color)]()

  def buildTableView(labeledTable: LabeledDataTable, model: Model): Unit = {

    if (!labeledTable.rebuild) {
      println("Changing table...")

      table.items = labeledTable.data
      return
    }

    println("Building table...")

    // initialize and add the table
    streamTable = TableViewBuilder.build(labeledTable)
    table = streamTable.table

    AnchorPane.setAnchors(table, 0, 0, 0, 0)
    tableContainer.content = List(table)

    // streamTable.onRightClick.subscribe(_ => println("right click"))
    // streamTable.onSort.subscribe(_ => println("right click"))

    // forward edits
    streamTable.onCellEdit.subscribe(x => onCellEdit.onNext(x))

    // A stream with the first selected cell
    val singleSelectedCell = streamTable.onSelection
      .filter(_.size == 1)
      .map(_.head)

    val sheetWithSelectedCell = model.sheet.combineLatest(singleSelectedCell)

    // Update the formula editor
    sheetWithSelectedCell
      .map(x => x match {
        case (sheet, pos) => sheet.cells.get(pos) match {
          case Some(cell) => cell.f
          case None => ""
        }
      })
      .distinctUntilChanged
      .subscribe(f => changeEditorText(f))

    // Update color pickers when selection changes
    sheetWithSelectedCell
      .map(x => x match {
        case (sheet, pos) => sheet.styles.get(pos) match {
          case Some(style) => style
          case None => Styles.DEFAULT
        }
      })
      .distinctUntilChanged
      .subscribe(s => {
        changeBackgroundColorPicker(s.background)
        changeFontColorPicker(s.color)
      })

    // Changes on formula editor are pushed to the selected cell
    singleSelectedCell.combineLatest(formulaEditorStream)
      .distinctUntilChanged(_._2)
      .subscribe(x => onCellEdit.onNext(x))

    // Changes on the ColorPickers are pushed to the model
    streamTable.onSelection.combineLatest(backgroundColorStream)
      .distinctUntilChanged(_._2)
      .flatMap(x => Observable.from(x._1.map(i => (i, x._2))))
      .subscribe(x => onBackgroundChange.onNext(x))

    streamTable.onSelection.combineLatest(fontColorStream)
      .distinctUntilChanged(_._2)
      .flatMap(x => Observable.from(x._1.map(i => (i, x._2))))
      .subscribe(x => onColorChange.onNext(x))

    // Load - Save
    saveStream.map(x => {
        fileChooser.setTitle("Save destination")
        fileChooser
      })
      .map(chooser => chooser.showSaveDialog(tableContainer.scene.window.getValue))
      .filter(_ != null)
      .labelAlways(model.sheet)
      .subscribe(fs => {
          val (file, sheet) = fs
          Filer.saveCSV(file, sheet)
      })

    loadStream.map(x => {
        fileChooser.setTitle("Open file")
        fileChooser
      })
      .map(chooser => chooser.showOpenDialog(tableContainer.scene.window.getValue))
      .filter(_ != null)
      .map(file => Filer.loadCSV(file))
      .subscribe(data => ??? /* DataManager.populateDataModel(data) */)
  }

  def initialize(url: URL, rb: java.util.ResourceBundle) {

    //
    // Initialization of GUI object handles
    //

    tableContainer = new AnchorPane(tableContainerDelegate)
    testButton = new Button(testButtonDelegate)

    backgroundColorPicker = new ColorPicker(backgroundColorPickerDelegate)
    backgroundColorStream = Observable.apply[Color](o => {
      backgroundColorPicker.onAction = handle {
        o.onNext(backgroundColorPicker.value.value)
      }
    })

    fontColorPicker = new ColorPicker(fontColorPickerDelegate)
    fontColorStream = Observable.apply[Color](o => {
      fontColorPicker.onAction = handle {
        o.onNext(fontColorPicker.value.value)
      }
    })

    formulaEditor = new TextField(formulaEditorDelegate)
    formulaEditorStream = Observable.apply[String](o => {
      formulaEditor.onAction = handle {
        o.onNext(formulaEditor.getText)
      }
    })

    menuLoad = new MenuItem(menuLoadDelegate)
    loadStream = Observable.apply[String](o => {
      menuLoad.onAction = handle {
        o.onNext("temp.csv")
      }
    })

    menuSave = new MenuItem(menuSaveDelegate)
    saveStream = Observable.apply[String](o => {
      menuSave.onAction = handle {
        o.onNext("temp.csv")
      }
    })

  }

  /**
   * Extension functions for Rx Observables
   */
  implicit class ExtendRx[T](ob: Observable[T]) {
    /** Similar to combineLatest, but it emits iff this observable emits */
    def labelAlways[L](la: Observable[L]) =
      ob.timestamp
        .combineLatest(la)
        .distinctUntilChanged(_._1._1)
        .map(x => (x._1._2, x._2))
  }


  def changeEditorText(text: String) = formulaEditor.text = text

  def changeBackgroundColorPicker(color: Color) = backgroundColorPicker.value = color

  def changeFontColorPicker(color: Color) = fontColorPicker.value = color

}
