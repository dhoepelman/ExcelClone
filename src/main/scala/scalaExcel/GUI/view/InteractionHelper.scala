package scalaExcel.GUI.view

import _root_.rx.lang.scala.Observable
import scalafx.Includes._
import scalafx.scene.paint.Color
import scalaExcel._
import scalaExcel.GUI.data.DataCell
import scalafx.scene.input.{DataFormat, ClipboardContent, Clipboard}
import scalaExcel.rx.operators.WithLatest._
import scalafx.scene.control.ScrollBar
import javafx.scene.{control => jfxsc}

object InteractionHelper {

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
    initializeFormulaEditor(controller)

    initializeColorPickers(controller)

    initializeSavingAndLoading(controller)

    initializeClipboard(controller)

    initializeSorting(controller)
  }

  private def initializeFormulaEditor(controller: ViewManager) {
    // Selecting a single cell updates the formula editor
    controller.onSingleCellSelected
      .distinctUntilChanged
      .subscribe(single => controller.editorText = single._2.expression)

    // Changes on formula editor are pushed to the selected cells
    Observable[String](o => {
      controller.formulaEditor.onAction = handle {
        o.onNext(controller.editorText)
      }
    })
      .distinctWithAllLatest(controller.onSelection)
      .subscribe(controller.onCellEdit)
  }

  private def initializeColorPickers(controller: ViewManager) {
    // Selecting a single cell updates the background and color pickers
    controller.onSingleCellSelected
      .distinctUntilChanged
      .map(single => single._2.styles)
      .subscribe(s => {
      controller.backgroundColor = s.background
      controller.fontColor = s.color
    })

    // Changes on the background picker are pushed to the model
    Observable[Color](o => {
      controller.backgroundColorPicker.onAction = handle {
        o.onNext(controller.backgroundColor)
      }
    })
      .withLatest(controller.onSelection)
      .subscribe(controller.onBackgroundChange)

    //Changes on the color picker are pushed to the model
    Observable[Color](o => {
      controller.fontColorPicker.onAction = handle {
        o.onNext(controller.fontColorPicker.value.value)
      }
    })
      .withLatest(controller.onSelection)
      .subscribe(controller.onColorChange)
  }

  private def initializeSavingAndLoading(controller: ViewManager) {
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
      .subscribe(controller.onLoad)

    // Emptying of cells is pushed to the model
    Observable[Unit](o =>
      controller.menuDelete.onAction = handle {
        o.onNext(Unit)
      })
      .withOnlyLatest(controller.onSelection)
      .subscribe(controller.onCellEmpty)
  }

  sealed trait ClipboardAction extends Serializable
  case object Cut extends ClipboardAction
  case object Copy extends ClipboardAction
  case object Paste extends ClipboardAction

  val copyPasteFormat = new DataFormat("x-excelClone/cutcopy")
  private def initializeClipboard(controller: ViewManager) {
    // Copy-pasting is handled here
    val clipboardActions = Observable[ClipboardAction](o => {
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
      .filter({ case (selection, _) => selection.nonEmpty})

    // TODO: Give the cell a visual indication that is is going to be cut, like Excel does
    clipboardActions
      .filter({ case (_, action) => action == Cut || action == Copy})
      // TODO: Multiple selection
      .map({ case (selection, action) => (selection.head, action)})
      .subscribe({ a =>
      println("Boe")
      // Pattern matching won't work. I give up
      val ((selection, cell), action) = a
      val contents = new ClipboardContent()
      contents.put(copyPasteFormat, (action, selection))
      contents.putString(cell.toString)
      Clipboard.systemClipboard.setContent(contents)
    })

    clipboardActions
      .subscribe({ a =>
      val (_, action) = a
      println(action)
      println(Clipboard.systemClipboard.getContent(copyPasteFormat))
    })

    clipboardActions
      .filter({ case (_, action) => action == Paste})
      // TODO: Multiple selection
      .map({ case (selection, action) => (selection.head, action)})
      .subscribe({ a =>
      // Pattern matching won't work. I give up
      val (selection, action) = a
      val clipboard = Clipboard.systemClipboard
      if (clipboard.hasContent(copyPasteFormat)) {
        clipboard.getContent(copyPasteFormat) match {
          case (Cut, from) =>
            // Cut-Pasting can only happen once
            clipboard.clear()
            controller.onCellCut.onNext((from.asInstanceOf[CellPos], selection._1))
          case (Copy, from) =>
            controller.onCellCopy.onNext((from.asInstanceOf[CellPos], selection._1))
          case other =>
            throw new IllegalArgumentException("Clipboard contained invalid copy-paste data {" + other.toString + "}")
        }
      } else if (clipboard.hasString)
        controller.onCellEdit.onNext((selection._1, clipboard.getString))
    })
  }

  private def initializeSorting(controller: ViewManager) {
    // Sorting of columns is pushed to the model
    Observable[Boolean](o => {
      controller.sortUp.onAction = handle {
        o.onNext(true)
      }
      controller.sortDown.onAction = handle {
        o.onNext(false)
      }
    })
      .withLatest(controller.onSingleCellSelected)
      .map(s => s match {
      case (((c, r), _), asc) => (c, asc)
    })
      .subscribe(controller.onColumnSort)
  }
}
