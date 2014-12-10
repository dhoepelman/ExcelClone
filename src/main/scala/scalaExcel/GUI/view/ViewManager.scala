package scalaExcel.GUI.view

import scala.language.reflectiveCalls
import java.net.URL
import rx.lang.scala._

import javafx.{event => jfxe, fxml => jfxf}
import javafx.scene.{control => jfxsc, layout => jfxsl}
import javafx.stage.FileChooser.ExtensionFilter

import scalafx.Includes._
import scalafx.scene.control._
import scalafx.scene.input.ScrollEvent
import scalafx.scene.layout.AnchorPane
import scalafx.scene.paint.Color

import scalaExcel.CellPos
import scalaExcel.GUI.data._
import scalaExcel.GUI.data.DataWindow.Bounds
import scalaExcel.GUI.data.LabeledDataTable.DataRow
import scalaExcel.GUI.view.InteractionHelper.WatchableScrollBar
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

  @jfxf.FXML private var menuUndoDelegate: jfxsc.MenuItem = _
  var menuUndo : MenuItem = _
  @jfxf.FXML private var menuRedoDelegate: jfxsc.MenuItem = _
  var menuRedo : MenuItem = _

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

  @jfxf.FXML private var horizontalScrollDelegate: jfxsc.ScrollBar = _
  private var horizontalScroll: WatchableScrollBar = _
  @jfxf.FXML private var verticalScrollDelegate: jfxsc.ScrollBar = _
  private var verticalScroll: WatchableScrollBar = _

  val fileChooser = new javafx.stage.FileChooser
  fileChooser.getExtensionFilters.addAll(
    new ExtensionFilter("ScalaExcel homebrew", "*.scalaexcel"),
    new ExtensionFilter("Comma separated values", "*.csv")
  )

  // Exposed observers, so we can gather those events and put
  // them into the model

  val onCellEdit = Subject[(CellPos, String)]()
  val onBackgroundChange = Subject[(Traversable[CellPos], Color)]()
  val onColorChange = Subject[(Traversable[CellPos], Color)]()
  val onColumnSort = Subject[(Int, Boolean)]()
  val onCellEmpty = Subject[Traversable[CellPos]]()
  val onCellCut = Subject[(CellPos, CellPos)]()
  val onCellCopy = Subject[(CellPos, CellPos)]()
  val onLoad = Subject[java.io.File]()
  val onUndo = Subject[Unit]()
  val onRedo = Subject[Unit]()
  val onAdd = Subject[(Boolean, Int, Int)]()
  val onRemove = Subject[(Boolean, Int, Int)]()
  val onColumnReorder = Subject[Map[Int, Int]]()

  /**
   * Rx stream of changes to the visible table
   */
  val tableMutations = Subject[TableMutations]()

  /**
   * Rx stream of wrappers on the data model sheet
   */
  val labeledDataTable = tableMutations.scan(new LabeledDataTable(rebuild = true))((dataTable, action) => {
    action match {
      case SlideWindowBy(offsets) => dataTable.slideWindowBy(offsets)
      case SlideWindowTo(bounds) => dataTable.slideWindowTo(bounds)
      case AddColumns(count, index) => dataTable.addColumns(count, index)
      case AddRows(count, index) => dataTable.addRows(count, index)
      case RemoveColumns(count, index) => dataTable.removeColumns(count, index)
      case RemoveRows(count, index) => dataTable.removeRows(count, index)
      case UpdateContents(newSheet) => dataTable.updateContents(newSheet)
      case UpdateColumnOrder(permutations) => dataTable.updateColumnOrder(permutations)
      case ResizeColumn(columnIndex, width) =>
        val newTable = dataTable.resizeColumn(columnIndex, width)
        val availableWidth = tableContainer.width.value
        if (dataTable.fitColumns(availableWidth) != newTable.fitColumns(availableWidth))
          newTable.layOut(availableWidth, tableContainer.height.value)
        else
          newTable
      case LayOutTable =>
        dataTable.layOut(tableContainer.width.value, tableContainer.height.value)
    }
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
    .map {
      case (pos, labeledTable) => (pos, labeledTable.dataCellFromSheet(pos))
    }

  /**
   * Stream with all selected cell data
   * type: Observable[List[(CellPos, DataCell)]]
   */
  val onManyCellsSelected = onSelection
    .combineLatest(labeledDataTable)
    .map {
      case (posList, labeledTable) =>
        posList.map(pos => (pos, labeledTable.dataCellFromSheet(pos)))
    }

  /**
   * Builds the visible table or only updates its contents
   * @param labeledTable  the latest LabeledDataTable
   */
  def buildTableView(labeledTable: LabeledDataTable): Unit = {

    //
    // Update TableView
    //

    // if only data changed, update items only
    if (!labeledTable.rebuild) {
      println("Changing table...")

      table.items = labeledTable.data
      return
    }

    println("Building table...")

    // otherwise initialize and add the table
    val streamTable = TableViewBuilder.build(labeledTable)

    table = streamTable.table
    AnchorPane.setAnchors(table, 0, 0, 0, 0)
    tableContainer.content = List(table)

    //
    // Re-subscribe scroll listener on table
    //

    Observable[(Double, Double)](o => {
      table.onScroll = (event: ScrollEvent) =>
        o.onNext((event.deltaX, event.deltaY))
    })
    // abort if sizes are 0 (table under construction)
    .filter(_ => table.width.value > 0  && table.height.value > 0)
    // calculate scroll amount as percentage of table width
    .map(deltas => (deltas._1/table.width.value, deltas._2/table.height.value))
    // calculate a proportional change of scroll bar value
    .map(percents => (horizontalScroll.value.value - percents._1 * horizontalScroll.max.value,
        verticalScroll.value.value - percents._2 * verticalScroll.max.value))
    .subscribe(amounts => {
      // apply change
      horizontalScroll.value = Math.max(0, Math.min(amounts._1, horizontalScroll.max.value))
      verticalScroll.value = Math.max(0, Math.min(amounts._2, verticalScroll.max.value))
    })


    // re-subscribe column width listener on table
    streamTable.onColResize.subscribe(resize =>
      tableMutations.onNext(ResizeColumn(resize._1, resize._2)))


    //
    // Forward actions on the streamTable's streams
    //

    // forward selection
    streamTable.onSelection.subscribe(onSelection.onNext _)
    //forward bulk selection
    streamTable.onBulkSelection.subscribe(s => {
      val indexes = s match{
        case (true, index) =>
          List.range(0, labeledTable.gridSize.columnCount).map(c => (c, index))
        case (false, index) =>
          List.range(0, labeledTable.gridSize.rowCount).map(r => (index, r))
      }
      onSelection.onNext(indexes)
    })
    // forward edits
    streamTable.onCellEdit.subscribe(onCellEdit.onNext _)
    // forward additions
    streamTable.onAdd.subscribe(s => {
      // first update the table's data window
      s match {
        case (true, count, index) =>
          tableMutations.onNext(AddRows(count, index))
        case (false, count, index) =>
          tableMutations.onNext(AddColumns(count, index))
      }
      // then update the model
      onAdd.onNext(s)
      })
    // forward removals
    streamTable.onRemove.subscribe(s => {
      // first update the table's data window
      s match {
        case (true, count, index) =>
          tableMutations.onNext(RemoveRows(count, index))
        case (false, count, index) =>
          tableMutations.onNext(RemoveColumns(count, index))
      }
      // the update the model
      onRemove.onNext(s)
      })
    // forward column reordering
    streamTable.onColumnReorder.subscribe(map => {
      // first update the table's inner header registry
      tableMutations.onNext(UpdateColumnOrder(map))
      // the update the model
      onColumnReorder.onNext(map)
    })

    //
    // Re-initialize scroll bars
    //

    val maxs = labeledTable.windowMaxOffsets
    val values = labeledTable.windowOffsets

    // stop listening to changes on the old bars
    if (horizontalScroll != null)
      horizontalScroll.unWatch()
    horizontalScroll = new WatchableScrollBar(horizontalScrollDelegate,
      maxs._1,
      values._1,
      (newValue: Int) =>
      // slide table window horizontally by the difference
        tableMutations.onNext(SlideWindowBy(Bounds(newValue - values._1, newValue - values._1, 0, 0))))

    if (verticalScroll != null)
      verticalScroll.unWatch()
    verticalScroll = new WatchableScrollBar(verticalScrollDelegate,
      maxs._2,
      values._2,
      (newValue: Int) =>
      // slide table window vertically by the difference
        tableMutations.onNext(SlideWindowBy(Bounds(0, 0, newValue - values._2, newValue - values._2))))

  }

  /**
   * To be called when the data model contents have changed
   * @param sheet the new data model sheet
   */
  def dataChanged(sheet: Sheet) =
    tableMutations.onNext(UpdateContents(sheet))

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
    menuRedo = new MenuItem(menuRedoDelegate)
    menuUndo = new MenuItem(menuUndoDelegate)
    menuCut = new MenuItem(menuCutDelegate)
    menuCopy = new MenuItem(menuCopyDelegate)
    menuPaste = new MenuItem(menuPasteDelegate)
    menuDelete = new MenuItem(menuDeleteDelegate)

    // initialize interaction streams
    InteractionHelper.initializeInteractionStreams(this)

    // subscribe table to data changes
    labeledDataTable.subscribe(buildTableView _)

    // handle changes on size of table container
    tableContainer.width.onChange {
      (_, _, newWidth) => {
        // re-render the table
        tableMutations.onNext(LayOutTable)
      }
    }
    tableContainer.height.onChange {
      (_, _, newHeight) => {
        // re-render the table
        tableMutations.onNext(LayOutTable)
      }
    }

    // handle undo/redo
    menuUndo.onAction = handle {
      onUndo.onNext(Unit)
    }
    menuRedo.onAction = handle {
      onRedo.onNext(Unit)
    }

  }

  def editorText = formulaEditor.text.value

  def editorText_=(text: String): Unit = formulaEditor.text = text

  def backgroundColor = backgroundColorPicker.value.value

  def backgroundColor_=(color: Color): Unit = backgroundColorPicker.value = color

  def fontColor = fontColorPicker.value.value

  def fontColor_=(color: Color): Unit = fontColorPicker.value = color

}
