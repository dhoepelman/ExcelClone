package scalaExcel.GUI

import scalafx.Includes._
import scalafx.application.JFXApp
import javafx.{fxml => jfxf}
import javafx.{scene => jfxs}
import java.io.IOException
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalaExcel.GUI.view.ViewManager
import scalaExcel.model._
import scalaExcel.model.Filer._
import scalafx.scene.control.Button
import scalafx.scene.layout.BorderPane
import scalafx.stage.Stage
import rx.lang.scala.Observable

object RunGUI extends JFXApp {

  val resource = getClass.getResource("/MainContainer.fxml")

  if (resource == null) {
    throw new IOException("Cannot load resource: MainContainer.fxml")
  }

  val loader = new jfxf.FXMLLoader(resource)
  val root = loader.load[jfxs.Parent]

  /**
   * The view
   */
  val vm = loader.getController[ViewManager]

  // Putting events from the GUI into the model
  // This should be the only place where that ever happens

  val modelChanges = observableMerge(
    // Make sure we immediately get the empty model
    Observable.just(Refresh),
    // when the user somehow changes the cell
    vm.onCellEdit.map(edit => SetFormula(edit._1, edit._2)),
    // when the background color of a cell is selected
    vm.onBackgroundChange.map(edit => SetBackground(edit._1, edit._2)),
    // when the front color of a cell is selected
    vm.onColorChange.map(edit => SetColor(edit._1, edit._2)),
    // when a column is sorted
    vm.onColumnSort.map(s => SortColumn(s._1, s._2)),
    // When a cell is deleted
    vm.onCellEmpty.map(pos => EmptyCell(pos)),
    // when a cell is copied
    vm.onCellCopy.map(exchange => CopyCell(exchange._1, exchange._2)),
    // when a cell is cut
    vm.onCellCut.map(exchange => CutCell(exchange._1, exchange._2)),
    // when a file is loaded
    vm.onLoad.map({ file =>
      val data = Filer.load(file)
      SetSheet(data._1, data._2)
    }),
    // when an action is undone
    vm.onUndo.map({ _ => Undo}),
    // when an action is redone
    vm.onRedo.map({ _ => Redo})
  )

  // The data model
  val model = new Model(modelChanges)

  // notify ViewManager of modifications in the model
  model.sheet.subscribe(vm.dataChanged _)

  // Show a dialog on errors and print the exception stack trace
  model.errors.subscribe({ e =>
    println("Exception while altering model.")
    e.printStackTrace()

    showInDialog("Sorry, something went wrong while we were doing that! Look in the console for details")
  })

  // Initialize the JavaFX stage
  stage = new PrimaryStage() {
    title = "Scala Excel"
    scene = new Scene(root, 800, 600) {
      stylesheets add "MainStyle.css"
    }
  }

  // SOURCE: https://github.com/scalafx/ScalaFX-Tutorials/blob/master/stand-alone-dialog/src/main/scala/stand_alone_dialog/StandAloneFXDialogRunAndWait.scala
  /** Show a `message` in a dialog box, wait till dialog is closed */
  private def showInDialog(message: String) {
    // Create dialog
    val dialogStage = new Stage {
      outer =>
      title = "Error"
      scene = new Scene {
        root = new BorderPane {
          padding = Insets(25)
          bottom = new Button {
            text = message
            onAction = handle { outer.close() }
          }
        }
      }
    }

    // Show dialog
    dialogStage.show()
  }

  /** Merges multiple observables into 1 stream */
  private def observableMerge[T](os : Observable[T]*) = Observable.from(os).flatten
}
