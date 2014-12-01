package scalaExcel.GUI

import scalafx.Includes._
import scalafx.application.JFXApp
import javafx.{fxml => jfxf}
import javafx.{scene => jfxs}
import java.io.IOException
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalaExcel.GUI.view.ViewManager
import scalaExcel.GUI.data.DataManager
import scalaExcel.model.Model

object RunGUI extends JFXApp {

  val resource = getClass.getResource("/MainContainer.fxml")

  if (resource == null) {
    throw new IOException("Cannot load resource: MainContainer.fxml")
  }

  val loader = new jfxf.FXMLLoader(resource)
  val root = loader.load[jfxs.Parent]

  val model = new Model()

  val vm = loader.getController[ViewManager]
  val dm = new DataManager(model)

  // Putting events from the GUI into the model
  // This should be the only place where that ever happens

  vm.onCellEdit.subscribe { edit =>
    model.changeFormula(edit._1, edit._2)
  }

  vm.onBackgroundChange.subscribe { edit =>
    model.changeBackground(edit._1, edit._2)
  }

  vm.onColorChange.subscribe { edit =>
    model.changeColor(edit._1, edit._2)
  }

  vm.onColumnSort.subscribe { s =>
    model.sortColumn(s._1, s._2)
  }

  // rerender table after data/resize/scroll changes
  dm.labeledDataTable.subscribe(table => vm.dataChanged(table))

  stage = new PrimaryStage() {
    title = "Scala Excel"
    scene = new Scene(root, 800, 600) {
      stylesheets add "MainStyle.css"
    }
  }

}
