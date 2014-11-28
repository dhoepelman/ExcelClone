package scalaExcel.GUI.view

import java.net.URL
import javafx.scene.{control => jfxsc, layout => jfxsl}
import javafx.stage.FileChooser.ExtensionFilter
import javafx.{event => jfxe, fxml => jfxf}

import rx.lang.scala._

import scala.language.reflectiveCalls
import scalaExcel.GUI.data.LabeledDataTable
import scalaExcel.GUI.data.LabeledDataTable.DataRow
import scalaExcel.GUI.util.Filer
import scalaExcel.model.{CellPos, Model, Styles}
import scalafx.Includes._
import scalafx.scene.control._
import scalafx.scene.layout.AnchorPane
import scalafx.scene.paint.Color

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

  @jfxf.FXML private var menuCutDelegate: javafx.scene.control.MenuItem = _
  @jfxf.FXML private var menuCopyDelegate: javafx.scene.control.MenuItem = _
  @jfxf.FXML private var menuPasteDelegate: javafx.scene.control.MenuItem = _
  @jfxf.FXML private var menuDeleteDelegate: javafx.scene.control.MenuItem = _

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
      .subscribe(file => Filer.saveCSV(file, table.items.getValue))

    loadStream.map(x => {
      fileChooser.setTitle("Open file")
      fileChooser
    })
      .map(chooser => chooser.showOpenDialog(tableContainer.scene.window.getValue))
      .filter(_ != null)
      .map(file => Filer.loadCSV(file))
      .subscribe(data => ??? /* DataManager.populateDataModel(data) */)

    // TODO: Fix this and/or put it in a correct place
    def getCurrentCoords() = {
      // TODO: Multiple selection
      val selected = table.selectionModel.value.getSelectedCells.head
      // First column is row numbers
      val coords = (selected.getColumn - 1, selected.getRow)
      coords
    }

    menuCutDelegate.onAction = handle {
      clipboard = Some(Cut, getCurrentCoords())
    }

    menuCopyDelegate.onAction = handle {
      clipboard = Some(Copy, getCurrentCoords())
    }

    menuPasteDelegate.onAction = handle {
      if (clipboard.isDefined) {
        val to = getCurrentCoords()
        clipboard get match {
          case (Cut, from) => model.cutCell(from, to)
          case (Copy, from) => model.copyCell(from, to)
        }
      }
    }

    menuDeleteDelegate.onAction = handle {
      val coords = getCurrentCoords()
      model.emptyCell(coords._1, coords._2)
    }
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
    def labelAlways[L](la: Observable[L]) =
      ob.combineLatest(la)
        .distinctUntilChanged(_._1)
        .map(c => new {
        val value = c._1;
        val label = c._2
      })
  }

  def changeEditorText(text: String) = formulaEditor.text = text

  def changeBackgroundColorPicker(color: Color) = backgroundColorPicker.value = color

  def changeFontColorPicker(color: Color) = fontColorPicker.value = color

  // TODO: Put this on the system clipboard with Clipboard class.
  // TODO: Put the cell value on the systen clipboard for copy-pasting outside of our program
  private var clipboard: Option[(ClipboardAction, CellPos)] = None

  private sealed trait ClipboardAction
  private case object Cut extends ClipboardAction
  private case object Copy extends ClipboardAction
}
