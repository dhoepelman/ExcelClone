package scalaExcel.GUI.view

import java.net.URL
import javafx.scene.{control => jfxsc, layout => jfxsl}
import javafx.stage.FileChooser.ExtensionFilter
import javafx.{event => jfxe, fxml => jfxf}

import rx.lang.scala._

import scala.language.reflectiveCalls

import scalaExcel.GUI.data.LabeledDataTable
import scalaExcel.GUI.data.LabeledDataTable.DataRow
import scalaExcel.GUI.view.ViewManager._
import scalaExcel.model.{Filer, Sheet, Model, Styles}
import scalaExcel.model.Filer._
import scalaExcel.rx.operators.WithLatest._

import scalafx.Includes._
import scalafx.scene.control._
import scalafx.scene.input.{ClipboardContent, _}
import scalafx.scene.layout.AnchorPane
import scalafx.scene.paint.Color

import scalaExcel.CellPos

class ViewManager extends jfxf.Initializable {

  private var streamTable: StreamingTable = _
  private var table: TableView[DataRow] = _

  @jfxf.FXML private var tableContainerDelegate: jfxsl.AnchorPane = _
  private var tableContainer: AnchorPane = _

  @jfxf.FXML private var formulaEditorDelegate: jfxsc.TextField = _
  private var formulaEditor: TextField = _
  private var formulaEditorStream: Observable[String] = _

  @jfxf.FXML private var backgroundColorPickerDelegate: jfxsc.ColorPicker = _
  private var backgroundColorPicker: jfxsc.ColorPicker = _
  private var backgroundColorStream: Observable[Color] = _

  @jfxf.FXML private var fontColorPickerDelegate: jfxsc.ColorPicker = _
  private var fontColorPicker: jfxsc.ColorPicker = _
  private var fontColorStream: Observable[Color] = _

  @jfxf.FXML private var menuLoadDelegate: jfxsc.MenuItem = _
  private var menuLoad: jfxsc.MenuItem = _
  private var loadStream: Observable[String] = _

  @jfxf.FXML private var menuSaveDelegate: jfxsc.MenuItem = _
  private var menuSave: jfxsc.MenuItem = _
  private var saveStream: Observable[String] = _

  @jfxf.FXML private var menuUndoDelegate: jfxsc.MenuItem = _
  private var menuUndo : jfxsc.MenuItem = _
  @jfxf.FXML private var menuRedoDelegate: jfxsc.MenuItem = _
  private var menuRedo : jfxsc.MenuItem = _

  @jfxf.FXML private var menuCutDelegate: jfxsc.MenuItem = _
  private var menuCut: jfxsc.MenuItem = _
  @jfxf.FXML private var menuCopyDelegate: jfxsc.MenuItem = _
  private var menuCopy: jfxsc.MenuItem = _
  @jfxf.FXML private var menuPasteDelegate: jfxsc.MenuItem = _
  private var menuPaste: jfxsc.MenuItem = _
  private var clipboardStream: Observable[ClipboardAction] = _

  @jfxf.FXML private var menuDeleteDelegate: jfxsc.MenuItem = _
  private var menuDelete: jfxsc.MenuItem = _
  private var deleteStream: Observable[List[CellPos]] = _

  @jfxf.FXML private var sortUpDelegate: jfxsc.Button = _
  private var sortUp: jfxsc.Button = _
  @jfxf.FXML private var sortDownDelegate: jfxsc.Button = _
  private var sortDown: jfxsc.Button = _
  private var onSortButtonStream: Observable[Boolean] = _

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
  val onBackgroundChange = Subject[(Traversable[CellPos], Color)]()
  val onColorChange = Subject[(Traversable[CellPos], Color)]()
  val onColumnSort = Subject[(Int, Boolean)]()

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

    // forward edits
    streamTable.onCellEdit.subscribe(x => onCellEdit.onNext(x))

    val selectedCell =
      streamTable.singleSelectedCellStream
        .combineLatest(model.sheet)

    // Update the formula editor
    selectedCell
      .map({ case (selected, sheet) => sheet.getCell(selected).f })
      .distinctUntilChanged
      .subscribe(f => changeEditorText(f))

    // Update color pickers when selection changes
    selectedCell
      .map({ case (selected, sheet) => sheet.getCellStyle(selected)})
      .subscribe({ s =>
        changeBackgroundColorPicker(s.background)
        changeFontColorPicker(s.color)
      })

    // Changes on formula editor are pushed to the selected cell
    formulaEditorStream
      .distinctUntilChanged
      .withLatest(streamTable.singleSelectedCellStream)
      .subscribe({x => onCellEdit.onNext(x) })

    // Changes on the ColorPickers are pushed to the model
    streamTable.withSelectedCells(backgroundColorStream)
      .subscribe(x => onBackgroundChange.onNext(x))

    streamTable.withSelectedCells(fontColorStream)
      .subscribe(x => onColorChange.onNext(x))

    // Load - Save
    saveStream.map(x => {
      fileChooser.setTitle("Save destination")
      fileChooser
    })
      .map(chooser => chooser.showSaveDialog(tableContainer.scene.window.getValue))
      .filter(_ != null)
      .withLatest(model.sheet)
      .subscribe(fs => {
          val (sheet, file) = fs
          sheet.saveTo(file)
      })

    loadStream.map(x => {
      fileChooser.setTitle("Open file")
      fileChooser
    })
      .map(chooser => chooser.showOpenDialog(tableContainer.scene.window.getValue))
      .filter(_ != null)
      .subscribe(file => model.loadFrom(file))

    deleteStream = streamTable.withSelectedCellsOnly(Observable[Unit]( o =>
      menuDelete.onAction = handle {
        o.onNext(Unit)
      }
    ))
    deleteStream.subscribe( ps => ps foreach( p => model.emptyCell(p)) )

    menuUndo.onAction = handle { model.undo() }
    menuRedo.onAction = handle { model.redo() }

    // TODO:  Yeah, so putting it in a variable first works. But when I put it directly in the subscribe it doesn't?...
    val clipboardHandler : ((Sheet, List[CellPos], ClipboardAction)) => Unit = {case (s, ps,action) =>
      // Ignore if no cells are selected
      if(ps.isEmpty)
        return
      // TODO: Multiple selection
      // TODO: Make the cell immediately disappear when cut
      val clipboard = Clipboard.systemClipboard
      val contents = new ClipboardContent()
      action match {
        case Cut | Copy => {
          contents.put(copyPasteFormat, (action, ps.head))
          contents.putString(s.getValue(ps.head).toString)
          clipboard.setContent(contents)
        }
        case Paste => {
          val to = ps.head
          if(clipboard.hasContent(copyPasteFormat))
            clipboard.getContent(copyPasteFormat) match {
              case (Cut, from) => {
                // Cut-Pasting can only happen once
                clipboard.clear()
                model.cutCell(from.asInstanceOf[CellPos], to)
              }
              case (Copy, from) => model.copyCell(from.asInstanceOf[CellPos], to)
              case a => throw new IllegalArgumentException("Clipboard contained invalid copy-paste data {" + a.toString + "}")
            }
          else if(clipboard.hasString)
            model.changeFormula(to, clipboard.getString)
        }
      }
    }
    streamTable
      .withSelectedCells(clipboardStream)
      .withLatest(model.sheet)
      .map({ case (s, (ps, a)) => (s,ps,a) })
      .subscribe(clipboardHandler)

    onSortButtonStream
      .withLatest(streamTable.singleSelectedCellStream)
      .subscribe { s => s match {
        case ((x, y), asc) => onColumnSort.onNext((x, asc))
      }}
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
    backgroundColorStream = Observable[Color](o => {
      backgroundColorPicker.onAction = handle {
        o.onNext(backgroundColorPicker.value.value)
      }
    })

    fontColorPicker = new ColorPicker(fontColorPickerDelegate)
    fontColorStream = Observable[Color](o => {
      fontColorPicker.onAction = handle {
        o.onNext(fontColorPicker.value.value)
      }
    })

    sortUp = new Button(sortUpDelegate)
    sortDown = new Button(sortDownDelegate)
    onSortButtonStream = Observable[Boolean](o => {
      sortUp.onAction = handle {
        o.onNext(true)
      }
      sortDown.onAction = handle {
        o.onNext(false)
      }
    })

    formulaEditor = new TextField(formulaEditorDelegate)
    formulaEditorStream = Observable[String](o => {
      formulaEditor.onAction = handle {
        o.onNext(formulaEditor.getText)
      }
    })

    menuLoad = new MenuItem(menuLoadDelegate)
    loadStream = Observable[String](o => {
      menuLoad.onAction = handle {
        o.onNext("temp.csv")
      }
    })

    menuSave = new MenuItem(menuSaveDelegate)
    saveStream = Observable[String](o => {
      menuSave.onAction = handle {
        o.onNext("temp.csv")
      }
    })

    menuRedo = new MenuItem(menuRedoDelegate)
    menuUndo = new MenuItem(menuUndoDelegate)

    menuCut = new MenuItem(menuCutDelegate)
    menuCopy = new MenuItem(menuCopyDelegate)
    menuPaste = new MenuItem(menuPasteDelegate)
    clipboardStream = Observable( o => {
      menuCut.onAction = handle {
        o.onNext(Cut)
      }
      menuCopy.onAction = handle {
        o.onNext(Copy)
      }
      menuPaste.onAction = handle {
        o.onNext(Paste)
      }
    }
    )

    menuDelete = new MenuItem(menuDeleteDelegate)
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
