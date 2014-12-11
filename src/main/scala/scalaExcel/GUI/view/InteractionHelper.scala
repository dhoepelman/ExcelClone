package scalaExcel.GUI.view

import _root_.rx.lang.scala.Observable
import scalafx.Includes._
import scalafx.scene.paint.Color
import scalaExcel._
import scalaExcel.GUI.data.DataCell
import scalafx.scene.input._
import scalaExcel.rx.operators.WithLatest._
import scalafx.scene.control._
import javafx.scene.{control => jfxsc}
import scalafx.scene.layout.{Priority, VBox, HBox, AnchorPane}
import scalafx.collections.ObservableBuffer
import scalafx.stage.{Modality, Stage, Window}
import scalafx.scene.{Node, Scene}
import scalafx.geometry.{Pos, Insets}
import scalaExcel.formula._

object InteractionHelper {

  sealed trait ClipboardAction extends Serializable
  case object Cut extends ClipboardAction
  case object Copy extends ClipboardAction
  case object Paste extends ClipboardAction

  /**
   * ScrollBar extension capable of emitting whole value changes
   * and cancelling emission on request
   */
  class WatchableScrollBar(delegate: jfxsc.ScrollBar,
                           maxValue: Int,
                           currentValue: Int,
                           valueListener: (Int) => Unit) extends ScrollBar(delegate) {
    max = maxValue
    value = currentValue
    blockIncrement = 5
    visibleAmount = if (maxValue == 1) 0.5 else 1

    val subscription = Observable[Int](o => {
        value.onChange {
          (_, _, newValue) =>
            if(!o.isUnsubscribed)
              o.onNext(newValue.doubleValue.round.toInt)
        }
    })
    .distinctUntilChanged
    .filter(v => v != currentValue)
    .subscribe(v => valueListener(v))

    def unWatch() = subscription.unsubscribe()
  }

  /**
   * Initializes all GUI interaction streams
   */
  def initializeInteractionStreams(controller: ViewManager) {
    // Selecting a single cell updates the formula editor
    controller.onSingleCellSelected
      .distinctUntilChanged
      .subscribe(single => controller.editorText = single._2.expression)

    // Selecting a single cell updates the background and color pickers
    controller.onSingleCellSelected
      .distinctUntilChanged
      .map(single => single._2.styles)
      .subscribe(s => {
      controller.backgroundColor = s.background
      controller.fontColor = s.color
    })

    // Changes on formula editor are pushed to the selected cells
    Observable[String](o => {
      controller.formulaEditor.onAction = handle {
        o.onNext(controller.editorText)
      }
    })
    .distinctWithAllLatest(controller.onSelection)
    .subscribe(controller.onCellEdit.onNext _)

    // Changes on the background picker are pushed to the model
    Observable[Color](o => {
      controller.backgroundColorPicker.onAction = handle {
        o.onNext(controller.backgroundColor)
      }
    })
    .withLatest(controller.onSelection)
    .subscribe(controller.onBackgroundChange.onNext _)

    //Changes on the color picker are pushed to the model
    Observable[Color](o => {
      controller.fontColorPicker.onAction = handle {
        o.onNext(controller.fontColorPicker.value.value)
      }
    })
    .withLatest(controller.onSelection)
    .subscribe(controller.onColorChange.onNext _)

    // Saves are handled here
    Observable[String](o => {
      controller.menuSave.onAction = handle {
        o.onNext("temp.csv")
      }
    })
    .map(x => {
      controller.fileChooser.setTitle("Save destination")
      controller.fileChooser
    })
    .map(chooser => chooser.showSaveDialog(controller.tableContainer.scene.window.getValue))
    .filter(_ != null)
      .withLatest(controller.labeledDataTable)
      .subscribe(fs => fs._1.saveTo(fs._2))

    // Loads are handled here
    Observable[String](o => {
      controller.menuLoad.onAction = handle {
        o.onNext("temp.csv")
      }
    })
    .map(x => {
      controller.fileChooser.setTitle("Open file")
      controller.fileChooser
    })
    .map(chooser => chooser.showOpenDialog(controller.tableContainer.scene.window.getValue))
    .filter(_ != null)
    .subscribe(controller.onLoad.onNext _)

    // Emptying of cells is pushed to the model
    Observable[Unit](o =>
      controller.menuDelete.onAction = handle {
        o.onNext(Unit)
    })
    .withOnlyLatest(controller.onSelection)
    .subscribe(controller.onCellEmpty.onNext(_))

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
                  controller.onCellCut.onNext((from.asInstanceOf[CellPos], to))
                case (Copy, from) => controller.onCellCopy.onNext((from.asInstanceOf[CellPos], to))
                case a => throw new IllegalArgumentException("Clipboard contained invalid copy-paste data {" + a.toString + "}")
              }
            else if (clipboard.hasString)
              controller.onCellEdit.onNext((to, clipboard.getString))
        }
    }

    // Copy-pasting is handled here
    Observable[ClipboardAction](o => {
      controller.menuCut.onAction = handle {
        o.onNext(Cut)
      }
      controller.menuCopy.onAction = handle {
        o.onNext(Copy)
      }
      controller.menuPaste.onAction = handle {
        o.onNext(Paste)
      }
    })
    .withLatest(controller.onManyCellsSelected)
    .subscribe(clipboardHandler)

    // Sorting of columns is pushed to the model
    Observable[Boolean](o => {
      controller.sortUp.onAction = handle {
        o.onNext(true)
      }
      controller.sortDown.onAction = handle {
        o.onNext(false)
      }
    })
    .withLatest(controller.onSelection)
    .subscribe(_ match {
      case (list, asc) => controller.onColumnSort.onNext((list.head._1, asc))
    })
  }

  val copyPasteFormat = new DataFormat("x-excelClone/cutcopy")

  /**
   * Shows the context menu that should appear when the user clicks a column
   * header or a row number
   * @param forRows       target item type (true for row, false for column)
   * @param parent        the node which registered the click and contains the
   *                      textual representation of the index
   * @param position      the screen position of the click action
   * @param addHandler    the handler for ADD actions that takes the
   *                      parameters:
   *                      (numberOfItemsAdded, offsetOfFirstAddedItem)
   * @param removeHandler the handler for REMOVE actions that takes the
   *                      parameters:
   *                      (numberOfItemsToRemoved, offsetOfFirstRemovedItem)
   */
  def showContextMenu(forRows: Boolean,
                      parent: Node,
                      position: (Double, Double),
                      addHandler: (Int, Int) => Unit,
                      removeHandler: (Int, Int) => Unit) = {
    val itemType = if(forRows) "row" else "column"
    new ContextMenu(
      // the standard add-one action
      new MenuItem("Add " + itemType + " after") {
        onAction = handle{
          addHandler(1, 1)
        }
      },
      // the add-many action
      new MenuItem("Add " + itemType + "(s)...") {
        onAction = handle{
          showAddRemoveDialog(isAdd = true,
            forRows,
            parent.scene.value.window.value,
            addHandler)
        }
      },
      // the standard remove-one action
      new MenuItem("Remove " + itemType) {
        onAction = handle {
          removeHandler(1, 0)
        }
      },
      // the remove-many action
      new MenuItem("Remove " + itemType + "(s)...") {
        onAction = handle{
          showAddRemoveDialog(isAdd = false,
            forRows,
            parent.scene.value.window.value,
            removeHandler)
        }
      }
    ).show(parent, position._1, position._2)
  }

  /**
   * Shows the dialog for adding/removing rows/columns
   * @param isAdd           the action type (true for ADD false for REMOVE)
   * @param forRows         the item type (true for row, false for column)
   * @param dialogOwner     the master window element
   * @param responseHandler the action handler that takes the parameters:
   *                        (numberOfItems, offsetOfFirstItem)
   */
  def showAddRemoveDialog(isAdd: Boolean,
                          forRows: Boolean,
                          dialogOwner: Window,
                          responseHandler: (Int, Int) => Unit) = {
    val radioToggle = new ToggleGroup()
    val countInput = new TextField(){
      text = "10"; prefColumnCount = 5
    }
    val itemType = if(forRows) "Row" else "Column"
    val actionType = if(isAdd) "Add" else "Remove"
    val noOffset = if(isAdd) "Before" else "From"
    val withOffset =  if(isAdd) "After" else "Until"
    new Stage(){
      title = "Add " + itemType +"(s)"
      initModality(Modality.APPLICATION_MODAL)
      initOwner(dialogOwner)
      scene = new Scene(
        new VBox(20){
          padding = Insets.apply(10, 10, 10, 10)
          spacing = 10
          content = ObservableBuffer(
            new Label(itemType + "s", countInput)
            {
              contentDisplay = ContentDisplay.Right
            },
            new Label(actionType, new VBox(2){
              content = ObservableBuffer(
                new RadioButton(noOffset){
                  toggleGroup = radioToggle
                  selected = true
                  userData = "0"
                },
                new RadioButton(withOffset) {
                  toggleGroup = radioToggle
                  userData = "1"
                }
              )
              spacing = 5
            })
            {
              contentDisplay = ContentDisplay.Right
            },
            new AnchorPane(){
              content = new HBox(2) {
                content = ObservableBuffer(
                  new Button(actionType) {
                    onAction = handle {
                      // get number of items
                      val count = countInput.text.value.toInt
                      val data = radioToggle.selectedToggle.value.userData.asInstanceOf[String]
                      // get the offset
                      val offset = data.toInt
                      responseHandler(count,
                        // on ADD, the offset is either 0 or 1
                        if(isAdd) offset
                        // on REMOVE the offset is either 0 or -(count - 1)
                        else (1 - count) * offset)
                      scene.value.getWindow.hide()
                    }
                  },
                  new Button("Cancel") {
                    onAction = handle {
                      scene.value.getWindow.hide()
                    }
                  }
                )
                spacing = 5
                alignment = Pos.BottomRight
              }
              vgrow = Priority.Always
              AnchorPane.setAnchors(content.get(0), 0, 0, 0, 0)
            }
          )
        },
        300,
        200
      )
    }.show()
  }

  /**
   * Returns a MouseEvent handler that can initiate operations on the set of
   * all rows/columns
   * @param forRows   the collection type (true for rows, false for columns)
   * @param parent    the node which will register the click and contains the
   *                  textual representation of the collection index
   * @param onAdd     the handler for ADD actions that takes the parameters:
   *                  (numberOfItemsToBeAdded, smallestIndexOfAddedItems)
   * @param onRemove  the handler for REMOVE actions that takes the
   *                  parameters:
   *                  (numberOfItemsToBeRemoved, smallestIndexOfRemovedItems)
   * @param onSelect  the handler for the SELECT action that takes the selected
   *                  collection index as a parameter
   * @return          the MouseEvent handler
   */
  def bulkOperationsInitiator(forRows: Boolean,
                             parent: Labeled,
                             onAdd: (Int, Int) => Unit,
                             onRemove: (Int, Int) => Unit,
                             onSelect: (Int) => Unit) =
    (event: MouseEvent) => if (parent.text.value != null) {
      // get selected index according to collection type
      val index =
        if(forRows) parent.text.value.toInt - 1
        else colToNum(parent.text.value)
      if(event.button == MouseButton.SECONDARY){
        // if right click, show context menu
        InteractionHelper.showContextMenu(forRows,
          parent,
          (event.screenX, event.screenY),
          (count: Int, offset: Int) => onAdd(count, index + offset),
          (count: Int, offset: Int) => {
            val (finalIndex, finalCount) = {
              // do not allow negative removal indexes
              val newIndex = index + offset
              if(newIndex < 0)
                (0, index + 1)
              else
                (newIndex, count)
            }
            onRemove(finalCount, finalIndex)
          }
        )
      }
      else
        // on left click select collection
        onSelect(index)
  }

}
