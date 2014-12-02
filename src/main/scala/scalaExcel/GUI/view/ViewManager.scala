package scalaExcel.GUI.view

import java.net.URL
import javafx.scene.{control => jfxsc, layout => jfxsl}
import javafx.stage.FileChooser.ExtensionFilter
import javafx.{event => jfxe, fxml => jfxf}

import rx.lang.scala._

import scala.language.reflectiveCalls

import scalaExcel.GUI.data._
import scalaExcel.GUI.data.LabeledDataTable.DataRow


import scalafx.Includes._
import scalafx.scene.control._
import scalafx.scene.layout.AnchorPane
import scalafx.scene.paint.Color

import scalaExcel.CellPos
import scalaExcel.GUI.data.SlideWindowTo
import scalaExcel.GUI.data.ResizeColumn
import scalaExcel.GUI.data.UpdateColumnOrder
import scalaExcel.GUI.data.UpdateContents
import scalaExcel.GUI.data.SlideWindowBy
import scalaExcel.model.Sheet

class ViewManager extends jfxf.Initializable {

   var table: TableView[DataRow] = _

  @jfxf.FXML private var tableContainerDelegate: jfxsl.AnchorPane = _
   var tableContainer: AnchorPane = _

  @jfxf.FXML private var formulaEditorDelegate: jfxsc.TextField = _
   var formulaEditor: TextField = _

  @jfxf.FXML private var backgroundColorPickerDelegate: jfxsc.ColorPicker = _
   var backgroundColorPicker: jfxsc.ColorPicker = _

  @jfxf.FXML private var fontColorPickerDelegate: jfxsc.ColorPicker = _
   var fontColorPicker: ColorPicker = _

  @jfxf.FXML private var menuLoadDelegate: jfxsc.MenuItem = _
   var menuLoad: MenuItem = _

  @jfxf.FXML private var menuSaveDelegate: jfxsc.MenuItem = _
   var menuSave: MenuItem = _

  @jfxf.FXML private var menuCutDelegate: jfxsc.MenuItem = _
   var menuCut: MenuItem = _
  @jfxf.FXML private var menuCopyDelegate: jfxsc.MenuItem = _
   var menuCopy: MenuItem = _
  @jfxf.FXML private var menuPasteDelegate: jfxsc.MenuItem = _
   var menuPaste: MenuItem = _

  @jfxf.FXML private var menuDeleteDelegate: jfxsc.MenuItem = _
   var menuDelete: MenuItem = _

  @jfxf.FXML private var sortUpDelegate: jfxsc.Button = _
   var sortUp: Button = _
  @jfxf.FXML private var sortDownDelegate: jfxsc.Button = _
   var sortDown: Button = _

  @jfxf.FXML
  private var testButtonDelegate: jfxsc.Button = _
  private var testButton: Button = _

  @jfxf.FXML private var newColumnDelegate: jfxsc.Button = _
  private var newColumnButton: Button = _
  @jfxf.FXML private var newRowDelegate: jfxsc.Button = _
  private var newRowButton: Button = _
  @jfxf.FXML private var scrollLeftDelegate: jfxsc.Button = _
  private var scrollLeftButton: Button = _
  @jfxf.FXML private var scrollRightDelegate: jfxsc.Button = _
  private var scrollRightButton: Button = _

  @jfxf.FXML private var scrollUpDelegate: jfxsc.Button = _
  private var scrollUpButton: Button = _
  @jfxf.FXML private var scrollDownDelegate: jfxsc.Button = _
  private var scrollDownButton: Button = _

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


  /**
   * Rx stream of changes to the visible table
   */
  val tableMutations = Subject[TableMutations]()

  /**
   * Rx stream of wrappers on the data model sheet
   */
  val labeledDataTable = tableMutations.scan(new LabeledDataTable(rebuild = true))((table, action) =>
    action match {
      case SlideWindowBy(offsets) => table.slideWindowBy(offsets)
      case SlideWindowTo(bounds) => table.slideWindowTo(bounds)
      case AddNewColumn(index) => table.addNewColumn(index)
      case AddNewRow(index) => table.addNewRow(index)
      case UpdateContents(newSheet) => table.updateContents(newSheet)
      case UpdateColumnOrder(permutations) => table.updateColumnOrder(permutations)
      case ResizeColumn(columnIndex, width) => table.resizeColumn(columnIndex, width)
      case RefreshTable() => table
    })

  /**
   * Global selection stream
   */
  val onSelection = Subject[List[CellPos]]()

  /**
   * Stream with the first selected cell data
   * type : Observable[(CellPos, DataCell)]
   */
  val onSingleCellSelected = onSelection
    .filter(_.size == 1)
    .map(_.head)
    .combineLatest(labeledDataTable)
    .map({
    case (pos, labeledTable) => (pos, labeledTable.dataCellFromSheet(pos))
  })

  /**
   * Stream with all selected cell data
   * type: Observable[List[(CellPos, DataCell)]]
   */
  val onManyCellsSelected = onSelection
    .combineLatest(labeledDataTable)
    .map({
    case (posList, labeledTable) =>
      posList.map(pos => (pos, labeledTable.dataCellFromSheet(pos)))
  })

  /**
   * Builds the visible table or only updates its contents
   * @param labeledTable  the latest LabeledDataTable
   */
  def buildTableView(labeledTable: LabeledDataTable): Unit = {

    if(!labeledTable.moreLeft)
      scrollLeftButton.disable = true
    else
      scrollLeftButton.disable = false

    if(!labeledTable.moreRight)
      scrollRightButton.disable = true
    else
      scrollRightButton.disable = false

    if(!labeledTable.moreUp)
      scrollUpButton.disable = true
    else
      scrollUpButton.disable = false

    if(!labeledTable.moreDown)
      scrollDownButton.disable = true
    else
      scrollDownButton.disable = false

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

  /**
   * To be called when the data model contents have changed
   * @param sheet the new data model sheet
   */
  def dataChanged(sheet: Sheet) =
    tableMutations.onNext(new UpdateContents(sheet))

  /**
   * Called on initialization of the FXML controller
   */
  def initialize(url: URL, rb: java.util.ResourceBundle) = {

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

    newColumnButton = new Button(newColumnDelegate)
    newRowButton = new Button(newRowDelegate)
    scrollLeftButton = new Button(scrollLeftDelegate)
    scrollRightButton = new Button(scrollRightDelegate)
    scrollUpButton = new Button(scrollUpDelegate)
    scrollDownButton = new Button(scrollDownDelegate)

    // initialize interaction streams
    InteractionHelper.initializeInteractionStreams(this)

    // subscribe table to data changes
    labeledDataTable.subscribe(buildTableView _)

    // start rendering the visible table
    tableMutations.onNext(new RefreshTable())

    newColumnButton.onAction = handle {
      // add new column at the end (position -1)
      tableMutations.onNext(new AddNewColumn(-1))
    }

    newRowButton.onAction = handle {
      // add new row at the end (position -1)
      tableMutations.onNext(new AddNewRow(-1))
    }

    scrollLeftButton.onAction = handle {
      // add new row at the end (position -1)
      tableMutations.onNext(new SlideWindowBy((-1, -1, 0, 0)))
    }

    scrollRightButton.onAction = handle {
      // add new row at the end (position -1)
      tableMutations.onNext(new SlideWindowBy((1, 1, 0, 0)))
    }

    scrollUpButton.onAction = handle {
      // add new row at the end (position -1)
      tableMutations.onNext(new SlideWindowBy((0, 0, -1, -1)))
    }

    scrollDownButton.onAction = handle {
      // add new row at the end (position -1)
      tableMutations.onNext(new SlideWindowBy((0, 0, 1, 1)))
    }
  }

  def editorText = formulaEditor.text.value
  def editorText_= (text: String):Unit = formulaEditor.text = text

  def backgroundColor = backgroundColorPicker.value.value
  def backgroundColor_= (color: Color): Unit = backgroundColorPicker.value = color

  def fontColor = fontColorPicker.value.value
  def fontColor_= (color:Color): Unit = fontColorPicker.value = color

}
