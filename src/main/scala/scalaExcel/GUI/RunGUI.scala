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
import scalaExcel.model.Model
import scalaExcel.model.Filer._
import scalafx.scene.control.Button
import scalafx.scene.layout.BorderPane
import scalafx.stage.Stage

object RunGUI extends JFXApp {

  val resource = getClass.getResource("/MainContainer.fxml")

  if (resource == null) {
    throw new IOException("Cannot load resource: MainContainer.fxml")
  }

  val loader = new jfxf.FXMLLoader(resource)
  val root = loader.load[jfxs.Parent]

  /**
   * The data model
   */
  val model = new Model()

  /**
   * The view
   */
  val vm = loader.getController[ViewManager]

  // notify ViewManager of modifications in the model
  model.sheet.subscribe(vm.dataChanged _)

  // Putting events from the GUI into the model
  // This should be the only place where that ever happens

  // when the user somehow changes the cell
  vm.onCellEdit.subscribe { edit =>
    model.changeFormula(edit._1, edit._2)
  }

  // when the background color of a cell is selected
  vm.onBackgroundChange.subscribe { edit =>
    model.changeBackground(edit._1, edit._2)
  }

  // when the front color of a cell is selected
  vm.onColorChange.subscribe { edit =>
    model.changeColor(edit._1, edit._2)
  }

  // when a column is sorted
  vm.onColumnSort.subscribe { s =>
    model.sortColumn(s._1, s._2)
  }

  // when a cell is emptied
  vm.onCellEmpty.subscribe { pos =>
    model.emptyCell(pos)
  }

  // when a cell is copied
  vm.onCellCopy.subscribe { exchange =>
    model.copyCell(exchange._1, exchange._2)
  }

  // when a cell is cut
  vm.onCellCut.subscribe { exchange =>
    model.cutCell(exchange._1, exchange._2)
  }

  // when a file is loaded
  vm.onLoad.subscribe { file =>
    model.loadFrom(file)
  }

  // when an action is undone
  vm.onUndo.subscribe { _ =>
    model.undo()
  }

  // when an action is redone
  vm.onRedo.subscribe { _ =>
    model.redo()
  }

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

  model.errors.subscribe({ e =>
    println("Exception while altering model.")
    e.printStackTrace()

    showInDialog("Sorry, something went wrong while we were doing that! Look in the console for details")
  })
}
