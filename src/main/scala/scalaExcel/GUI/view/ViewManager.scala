package scalaExcel.GUI.view


import rx.lang.scala.subjects.BehaviorSubject

import java.util.Locale

import scala.language.reflectiveCalls
import java.net.URL
import rx.lang.scala._

import javafx.{stage => jfxs, fxml => jfxf}
import javafx.scene.{control => jfxsc, layout => jfxsl}
import javafx.stage.FileChooser.ExtensionFilter

import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.input.ScrollEvent
import scalafx.scene.layout.AnchorPane
import scalafx.scene.paint.Color

import scalaExcel.CellPos
import scalaExcel.GUI.data._
import scalaExcel.GUI.data.LabeledDataTable.DataRow
import scalaExcel.GUI.view.InteractionHelper.WatchableScrollBar
import scalaExcel.model._
import scalaExcel.GUI.data.SlideWindowTo
import scalaExcel.GUI.data.DataWindow.Bounds
import scalaExcel.GUI.data.ResizeColumn
import scalaExcel.GUI.data.AddRows
import scalaExcel.GUI.data.RemoveRows
import scalaExcel.GUI.data.UpdateColumnOrder
import scalaExcel.GUI.data.AddColumns
import scalaExcel.GUI.data.UpdateContents
import scalaExcel.GUI.data.SlideWindowBy
import scalaExcel.GUI.data.RemoveColumns
import scalaExcel.formula.colToNum

class ViewManager extends jfxf.Initializable {

  var table: TableView[DataRow] = _

  @jfxf.FXML private var tableContainerDelegate: jfxsl.AnchorPane = _
  var tableContainer: AnchorPane = _

  @jfxf.FXML private var formulaEditorDelegate: jfxsc.TextField = _
  var formulaEditor: TextField = _

  @jfxf.FXML private var formulaCheatSheetDelegate: jfxsc.MenuButton = _
  var formulaCheatSheet: MenuButton = _

  @jfxf.FXML private var backgroundColorPickerDelegate: jfxsc.ColorPicker = _
  var backgroundColorPicker: jfxsc.ColorPicker = _

  @jfxf.FXML private var fontColorPickerDelegate: jfxsc.ColorPicker = _
  var fontColorPicker: ColorPicker = _

  @jfxf.FXML private var menuLoadDelegate: jfxsc.MenuItem = _
  var menuLoad: MenuItem = _

  @jfxf.FXML private var menuSaveDelegate: jfxsc.MenuItem = _
  var menuSave: MenuItem = _

  @jfxf.FXML private var menuNewDelegate: jfxsc.MenuItem = _
  var menuNew: MenuItem = _

  @jfxf.FXML private var menuCloseDelegate: jfxsc.MenuItem = _
  var menuClose: MenuItem = _

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

  @jfxf.FXML private var menuGraphDelegate: jfxsc.MenuItem = _
  var menuGraph: MenuItem = _

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

  @jfxf.FXML private var alignLeftDelegate: jfxsc.Button = _
  var alignLeftButton: Button = _
  @jfxf.FXML private var alignCenterDelegate: jfxsc.Button = _
  var alignCenterButton: Button = _
  @jfxf.FXML private var alignRightDelegate: jfxsc.Button = _
  var alignRightButton: Button = _

  @jfxf.FXML private var addColsDelegate: jfxsc.Button = _
  var addColsButton: Button = _
  @jfxf.FXML private var addRowsDelegate: jfxsc.Button = _
  var addRowsButton: Button = _

  @jfxf.FXML private var formatChoiceDelegate: jfxsc.ChoiceBox[ValueFormat] = _
  var formatChoice: ChoiceBox[ValueFormat] = _

  @jfxf.FXML private var localeMenuDelegate: jfxsc.Menu = _
  private var localeMenu: Menu = _

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
  val onAddColumns = Subject[(Int, Int)]()
  val onAddRows = Subject[(Int, Int)]()
  val onRemoveRows = Subject[(Int, Int)]()
  val onRemoveColumns = Subject[(Int, Int)]()
  val onColumnReorder = Subject[Map[Int, Int]]()
  val onNewSheet = Subject[Unit]()
  val onAlign = Subject[(Traversable[CellPos], Alignment)]()
  val onFormat = Subject[(Traversable[CellPos], ValueFormat)]()
  val onRefresh = Subject[Unit]()

  val sheetsForGraph = BehaviorSubject[Sheet]() // TODO create from existing streams

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
      case Reset => new LabeledDataTable(rebuild = true)
        .layOut(tableContainer.width.value, tableContainer.height.value)
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
    streamTable.onSelection.subscribe(onSelection)
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
    streamTable.onCellEdit.subscribe(onCellEdit)
    // forward additions
    streamTable.onAdd.subscribe(_ match {
      case (true, count, index) =>
        // first update the table's data window
        tableMutations.onNext(AddRows(count, index))
        // then update the model
        onAddRows.onNext((count, index))
      case (false, count, index) =>
        tableMutations.onNext(AddColumns(count, index))
        onAddColumns.onNext((count, index))
    })
    // forward removals
    streamTable.onRemove.subscribe(_ match {
      case (true, count, index) =>
        tableMutations.onNext(RemoveRows(count, index))
        onRemoveRows.onNext((count, index))
      case (false, count, index) =>
        tableMutations.onNext(RemoveColumns(count, index))
        onRemoveColumns.onNext((count, index))
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

    horizontalScroll = new WatchableScrollBar(
      horizontalScrollDelegate,
      maxs._1,
      values._1,
      (newValue: Int) => {
        val diff = newValue - values._1
        // do not allow a single scroll action to exceed window width
        val maxDiff = Math.signum(diff).toInt * Math.min(Math.abs(diff), labeledTable.windowSize.columnCount / 2)
        // slide table window horizontally by the difference
        tableMutations.onNext(SlideWindowBy(Bounds(maxDiff, maxDiff, 0, 0)))
      }
    )

    if (verticalScroll != null)
      verticalScroll.unWatch()

    verticalScroll = new WatchableScrollBar(
      verticalScrollDelegate,
      maxs._2,
      values._2,
      (newValue: Int) => {
          val diff = newValue - values._2
          // do not allow a single scroll action to exceed window height
          val maxDiff = Math.signum(diff).toInt * Math.min(Math.abs(diff), labeledTable.windowSize.rowCount / 2)
          // slide table window vertically by the difference
          tableMutations.onNext(SlideWindowBy(Bounds(0, 0, maxDiff, maxDiff)))
        }
    )

    addRowsButton.onAction = handle {
     streamTable.onAdd.onNext((true, 10, labeledTable.gridSize.rowCount))
    }

    addColsButton.onAction = handle {
      streamTable.onAdd.onNext((false, 5, labeledTable.gridSize.columnCount))
    }

    menuGraph.onAction = handle {
      InteractionHelper.showGraphDialog(tableContainer.scene.value.getWindow, {
        case (onColumns, indexString) =>
          val indexStringList = indexString.split(",").toList.map(_.trim)
          val indexLabelList =
            indexStringList.map({ case s =>
              try {
                if (onColumns)
                  labeledTable.toSheetIndex((colToNum(s), 0))._1
                else
                  labeledTable.toSheetIndex((0, s.toInt - 1))._2
              } catch {
                case _: Throwable => -1
              }
            })
              .zip(indexStringList) // index with preserved label
              .filter(_._1 >= 0)
          new GraphWindow(sheetsForGraph, onColumns, indexLabelList).show()
      })
    }

  }

  /**
   * To be called when the data model contents have changed
   * @param sheet the new data model sheet
   */
  def dataChanged(sheet: Sheet) = {
    tableMutations.onNext(UpdateContents(sheet))
    sheetsForGraph.onNext(sheet)
  }

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
    formulaCheatSheet = new MenuButton(formulaCheatSheetDelegate)
    menuLoad = new MenuItem(menuLoadDelegate)
    menuSave = new MenuItem(menuSaveDelegate)
    menuRedo = new MenuItem(menuRedoDelegate)
    menuUndo = new MenuItem(menuUndoDelegate)
    menuCut = new MenuItem(menuCutDelegate)
    menuCopy = new MenuItem(menuCopyDelegate)
    menuPaste = new MenuItem(menuPasteDelegate)
    menuDelete = new MenuItem(menuDeleteDelegate)
    menuNew = new MenuItem(menuNewDelegate)
    menuClose = new MenuItem(menuCloseDelegate)
    menuGraph = new MenuItem(menuGraphDelegate)
    alignLeftButton = new Button(alignLeftDelegate)
    alignCenterButton = new Button(alignCenterDelegate)
    alignRightButton = new Button(alignRightDelegate)
    addColsButton = new Button(addColsDelegate)
    addRowsButton = new Button(addRowsDelegate)
    formatChoice = new ChoiceBox(formatChoiceDelegate)
    formatChoice.items = ObservableBuffer(DefaultValueFormat,
      TextValueFormat,
      CurrencyValueFormat,
      ScientificValueFormat,
      PercentageValueFormat,
      new CustomNumericValueFormat())
    formatChoice.value = DefaultValueFormat
    localeMenu = new Menu(localeMenuDelegate)
    localeMenu.items = ObservableBuffer() ++ Locale.getAvailableLocales
      .filter(_.getDisplayLanguage != "")
      .filter(_.getDisplayCountry != "")
      .sortBy(_.getDisplayLanguage)
      .map({locale =>
        new MenuItem() {
          userData = locale
          text = locale.getDisplayLanguage + "(" + locale.getCountry + ")"
          onAction = handle {
            Locale.setDefault(userData.asInstanceOf[Locale])
            onRefresh.onNext(Unit)
          }
        }})

    val fcsItems = formulaCheatSheet.items

    List(
      "SUM(range)",
      "AVERAGE(range)",
      "POWER(num, exp)",
      "ROUND(num)",
      "ROW(ref)",
      "ROWS(range)",
      "COLUMN(ref)",
      "COLUMNS(range)",
      "COUNT(range)",
      "MATCH(value, range)",
      "VLOOKUP(value, range, offset)",
      "ADDRESS(row, col, abs)",
      "IF(test, true, false)",
      "OR(value1, value2)",
      "AND(value1, value2)",
      "NOT(value)",
      "UPPER(value)",
      "LOWER(value)",
      "LEN(value)",
      "TRIM(value)",
      "ISBLANK(value)",
      "ISERROR(value)",
      "ISNA(value)",
      "ISLOGICAL(value)",
      "ISNUMBER(value)",
      "ISTEXT(value)"
    ).foreach(fn => {
      val item = new MenuItem(fn)
      item.onAction = handle(editorText = editorText + fn)
      fcsItems.add(item)
    })

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

    menuNew.onAction = handle {
      onNewSheet.onNext(Unit)
      tableMutations.onNext(Reset)
    }

    menuClose.onAction = handle {
      tableContainer.scene.value.getWindow.asInstanceOf[jfxs.Stage].close()
    }

  }

  def editorText = formulaEditor.text.value

  def editorText_=(text: String): Unit = formulaEditor.text = text

  def backgroundColor = backgroundColorPicker.value.value

  def backgroundColor_=(color: Color): Unit = backgroundColorPicker.value = color

  def fontColor = fontColorPicker.value.value

  def fontColor_=(color: Color): Unit = fontColorPicker.value = color

  def alignment =
    if (alignRightButton.isDisabled) RightAlign
    else if (alignCenterButton.isDisabled) CenterAlign
    else LeftAlign

  def alignment_=(alignment: Alignment): Unit = alignment match {
    case LeftAlign =>
      alignLeftButton.disable = true
      alignCenterButton.disable = false
      alignRightButton.disable = false
    case RightAlign =>
      alignLeftButton.disable = false
      alignCenterButton.disable = false
      alignRightButton.disable = true
    case CenterAlign =>
      alignLeftButton.disable = false
      alignCenterButton.disable = true
      alignRightButton.disable = false
    case _ =>
      alignLeftButton.disable = false
      alignCenterButton.disable = false
      alignRightButton.disable = false
  }

  def formatting = formatChoice.value.value

  def formatting_=(format: ValueFormat): Unit = formatChoice.value = format

  def formattingEnabled = !formatChoice.isDisabled

  def formattingEnabled_=(enabled: Boolean): Unit = formatChoice.disable = !enabled

}
