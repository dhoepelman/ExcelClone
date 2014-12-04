package scalaExcel.GUI.view

import _root_.rx.lang.scala.{Subject, Observable}
import scalafx.Includes._
import scalafx.scene.paint.Color
import scalaExcel._
import scalaExcel.GUI.data.DataCell
import scalafx.scene.input.{DataFormat, ClipboardContent, Clipboard}
import scalaExcel.rx.operators.WithLatest._
import scalafx.scene.control.ScrollBar
import javafx.scene.{control => jfxsc}

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
    .subscribe(v => if(v != currentValue) valueListener(v))

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
    .distinctWithAllLatest(controller.onSelection)
    .subscribe(controller.onBackgroundChange.onNext _)

    //Changes on the color picker are pushed to the model
    Observable[Color](o => {
      controller.fontColorPicker.onAction = handle {
        o.onNext(controller.fontColorPicker.value.value)
      }
    })
    .distinctWithAllLatest(controller.onSelection)
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
    .distinctWithAllLatest(controller.onSelection)
    .subscribe(s => controller.onCellEmpty.onNext(s._1))

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
    .withLatest(controller.onSingleCellSelected)
    .subscribe(s => s match {
      case (((c, r), _), asc) => controller.onColumnSort.onNext((c, asc))
    })
  }

  val copyPasteFormat = new DataFormat("x-excelClone/cutcopy")

}
