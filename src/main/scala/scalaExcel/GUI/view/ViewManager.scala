package scalaExcel.GUI.view

import java.net.URL
import javafx.scene.{control => jfxsc, layout => jfxsl}
import javafx.stage.FileChooser.ExtensionFilter
import javafx.{event => jfxe, fxml => jfxf}

import rx.lang.scala._

import scala.language.reflectiveCalls

import scalaExcel.GUI.data.{DataCell, LabeledDataTable}
import scalaExcel.GUI.data.LabeledDataTable.DataRow
import scalaExcel.GUI.util.Filer
import scalaExcel.GUI.view.ViewManager._
import scalaExcel.rx.operators.WithLatest._

import scalafx.Includes._
import scalafx.scene.control._
import scalafx.scene.input.{ClipboardContent, _}
import scalafx.scene.layout.AnchorPane
import scalafx.scene.paint.Color

import scalaExcel.CellPos

class ViewManager extends jfxf.Initializable {

  private var table: TableView[DataRow] = _

  @jfxf.FXML private var tableContainerDelegate: jfxsl.AnchorPane = _
  private var tableContainer: AnchorPane = _

  @jfxf.FXML private var formulaEditorDelegate: jfxsc.TextField = _
  private var formulaEditor: TextField = _

  @jfxf.FXML private var backgroundColorPickerDelegate: jfxsc.ColorPicker = _
  private var backgroundColorPicker: jfxsc.ColorPicker = _

  @jfxf.FXML private var fontColorPickerDelegate: jfxsc.ColorPicker = _
  private var fontColorPicker: jfxsc.ColorPicker = _

  @jfxf.FXML private var menuLoadDelegate: jfxsc.MenuItem = _
  private var menuLoad: jfxsc.MenuItem = _

  @jfxf.FXML private var menuSaveDelegate: jfxsc.MenuItem = _
  private var menuSave: jfxsc.MenuItem = _

  @jfxf.FXML private var menuCutDelegate: jfxsc.MenuItem = _
  private var menuCut: jfxsc.MenuItem = _
  @jfxf.FXML private var menuCopyDelegate: jfxsc.MenuItem = _
  private var menuCopy: jfxsc.MenuItem = _
  @jfxf.FXML private var menuPasteDelegate: jfxsc.MenuItem = _
  private var menuPaste: jfxsc.MenuItem = _

  @jfxf.FXML private var menuDeleteDelegate: jfxsc.MenuItem = _
  private var menuDelete: jfxsc.MenuItem = _

  @jfxf.FXML private var sortUpDelegate: jfxsc.Button = _
  private var sortUp: jfxsc.Button = _
  @jfxf.FXML private var sortDownDelegate: jfxsc.Button = _
  private var sortDown: jfxsc.Button = _

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

  val onCellEdit = Subject[(CellPos, String)]()
  val onBackgroundChange = Subject[(CellPos, Color)]()
  val onColorChange = Subject[(CellPos, Color)]()
  val onColumnSort = Subject[(Int, Boolean)]()
  val onCellEmpty = Subject[CellPos]()
  val onCellCut = Subject[(CellPos, CellPos)]()
  val onCellCopy = Subject[(CellPos, CellPos)]()
  // global selection stream
  val onSelection = Subject[List[CellPos]]()
  // stream with cell data packaged as a LabeledDataTable
  val onDataChanged = Subject[LabeledDataTable]()
  // stream with the first selected cell data
  val onSingleCellSelected = onSelection
    .filter(_.size == 1)
    .map(_.head)
    .combineLatest(onDataChanged)
    .map({
    case (pos, labeledTable) => (pos, labeledTable.dataCellFromSheet(pos))
  })
  //stream with all selected cell data
  val onManyCellsSelected = onSelection
    .combineLatest(onDataChanged)
    .map({
    case (posList, labeledTable) =>
      posList.map(pos => (pos, labeledTable.dataCellFromSheet(pos)))
  })

  def buildTableView(labeledTable: LabeledDataTable): Unit = {

    if (!labeledTable.rebuild) {
      println("Changing table...")

      table.items = labeledTable.data
      return
    }

    println("Building table...")

    // initialize and add the table
    val streamTable = TableViewBuilder.build(labeledTable)
    table = streamTable.table

    AnchorPane.setAnchors(table, 0, 0, 0, 0)
    tableContainer.content = List(table)

    // forward selection
    streamTable.onSelection.subscribe(onSelection.onNext _)
    // forward edits
    streamTable.onCellEdit.subscribe(onCellEdit.onNext _)
  }

  def initializeStreams(): Unit = {

    // Selecting a single cell updates the formula editor
    onSingleCellSelected
      .distinctUntilChanged
      .subscribe(single => changeEditorText(single._2.expression))

    // Selecting a single cell updates the background and color pickers
    onSingleCellSelected
      .distinctUntilChanged
      .map(single => single._2.styles)
      .subscribe(s => {
      changeBackgroundColorPicker(s.background)
      changeFontColorPicker(s.color)
    })

    // Changes on formula editor are pushed to the selected cell
    Observable[String](o => {
      formulaEditor.onAction = handle {
        o.onNext(formulaEditor.getText)
      }
    })
      .withLatest(onSingleCellSelected)
      .distinctUntilChanged(_._2)
      .map({
      case ((pos, _), formula) => (pos, formula)
    })
      .subscribe(onCellEdit.onNext _)

    // Changes on the background picker are pushed to the model
    Observable[Color](o => {
      backgroundColorPicker.onAction = handle {
        o.onNext(backgroundColorPicker.value.value)
      }
    })
      .withLatest(onSelection)
      .distinctUntilChanged(_._2)
      .flatMap(x => Observable.from(x._1.map(i => (i, x._2))))
      .subscribe(onBackgroundChange.onNext _)

    //Changes on the color picker are pushed to the model
    Observable[Color](o => {
      fontColorPicker.onAction = handle {
        o.onNext(fontColorPicker.value.value)
      }
    })
      .withLatest(onSelection)
      .distinctUntilChanged(_._2)
      .flatMap(x => Observable.from(x._1.map(i => (i, x._2))))
      .subscribe(onColorChange.onNext _)

    // Saves are handled here
    Observable[String](o => {
      menuSave.onAction = handle {
        o.onNext("temp.csv")
      }
    }).map(x => {
      fileChooser.setTitle("Save destination")
      fileChooser
    })
      .map(chooser => chooser.showSaveDialog(tableContainer.scene.window.getValue))
      .filter(_ != null)
      .subscribe(file => Filer.saveCSV(file, table.items.getValue))

    // Loads are handled here
    Observable[String](o => {
      menuLoad.onAction = handle {
        o.onNext("temp.csv")
      }
    }).map(x => {
      fileChooser.setTitle("Open file")
      fileChooser
    })
      .map(chooser => chooser.showOpenDialog(tableContainer.scene.window.getValue))
      .filter(_ != null)
      .map(file => Filer.loadCSV(file))
      .subscribe(data => ??? /* DataManager.populateDataModel(data) */)

    // Emptying of cells is pushed to the model
    Observable[Unit](o =>
      menuDelete.onAction = handle {
        o.onNext(Unit)
      })
      .withLatest(onSelection)
      .map({
      case (pos, _) => pos
    })
      .subscribe(ps => ps foreach (p => onCellEmpty.onNext(p)))

    // Copy-pasting is handled by this function
    // TODO:  Yeah, so putting it in a variable first works. But when I put it directly in the subscribe it doesn't?...
    val clipboardHandler: ((List[(CellPos, DataCell)], ClipboardAction)) => Unit = {
      case (selection, action) =>
        // Ignore if no cells are selected
        if (selection.isEmpty)
          return
        // TODO: Multiple selection
        // TODO: Make the cell immediately disappear when cut
        val clipboard = Clipboard.systemClipboard
        val contents = new ClipboardContent()
        action match {
          case Cut | Copy =>
            contents.put(copyPasteFormat, (action, selection.head._1))
            contents.putString(selection.head._2.value.toString)
            clipboard.setContent(contents)
          case Paste =>
            val to = selection.head._1
            if (clipboard.hasContent(copyPasteFormat))
              clipboard.getContent(copyPasteFormat) match {
                case (Cut, from) =>
                  // Cut-Pasting can only happen once
                  clipboard.clear()
                  onCellCut.onNext((from.asInstanceOf[CellPos], to))
                case (Copy, from) => onCellCopy.onNext((from.asInstanceOf[CellPos], to))
                case a => throw new IllegalArgumentException("Clipboard contained invalid copy-paste data {" + a.toString + "}")
              }
            else if (clipboard.hasString)
              onCellEdit.onNext((to, clipboard.getString))
        }
    }

    // Copy-pasting is handled here
    Observable[ClipboardAction](o => {
      menuCut.onAction = handle {
        o.onNext(Cut)
      }
      menuCopy.onAction = handle {
        o.onNext(Copy)
      }
      menuPaste.onAction = handle {
        o.onNext(Paste)
      }
    })
      .withLatest(onManyCellsSelected)
      .subscribe(clipboardHandler)

    // Sorting of columns is pushed to the model
    Observable[Boolean](o => {
      sortUp.onAction = handle {
        o.onNext(true)
      }
      sortDown.onAction = handle {
        o.onNext(false)
      }
    })
      .withLatest(onSingleCellSelected)
      .subscribe(s => s match {
      case (((c, r), _), asc) => onColumnSort.onNext((c, asc))
    })
  }

  val copyPasteFormat = new DataFormat("x-excelClone/cutcopy")

  def initialize(url: URL, rb: java.util.ResourceBundle) {

    println("ViewManager initializing...")

    //
    // Initialization of GUI object handles
    //

    tableContainer = new AnchorPane(tableContainerDelegate)
    testButton = new Button(testButtonDelegate)
    backgroundColorPicker = new ColorPicker(backgroundColorPickerDelegate)
    fontColorPicker = new ColorPicker(fontColorPickerDelegate)
    sortUp = new Button(sortUpDelegate)
    sortDown = new Button(sortDownDelegate)
    formulaEditor = new TextField(formulaEditorDelegate)
    menuLoad = new MenuItem(menuLoadDelegate)
    menuSave = new MenuItem(menuSaveDelegate)
    menuCut = new MenuItem(menuCutDelegate)
    menuCopy = new MenuItem(menuCopyDelegate)
    menuPaste = new MenuItem(menuPasteDelegate)
    menuDelete = new MenuItem(menuDeleteDelegate)

    //
    // Initialization of GUI streams
    //

    initializeStreams()

    // subscribe table to data changes
    onDataChanged.subscribe(buildTableView _)
  }

  /**
   * Extension functions for Rx Observables
   */
  implicit class ExtendRx[T](ob: Observable[T]) {
    def labelAlways[L](la: Observable[L]) =
      ob.combineLatest(la)
        .distinctUntilChanged(_._1)
        .map(c => new {
        val value = c._1
        val label = c._2
      })
  }

  def changeEditorText(text: String) = formulaEditor.text = text

  def changeBackgroundColorPicker(color: Color) = backgroundColorPicker.value = color

  def changeFontColorPicker(color: Color) = fontColorPicker.value = color

}

object ViewManager {

  sealed trait ClipboardAction extends Serializable

  case object Cut extends ClipboardAction

  case object Copy extends ClipboardAction

  case object Paste extends ClipboardAction

}
