package scalaExcel.GUI

import scalafx.Includes._
import scalafx.application.JFXApp
import javafx.{fxml => jfxf}
import javafx.{scene => jfxs}
import java.io.IOException
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalaExcel.GUI.view.ViewManager
import scalaExcel.GUI.data._
import scalaExcel.model.Model
import scalaExcel.GUI.data.ChangeFormula
import scalaExcel.GUI.data.ChangeBackground
import scalaExcel.GUI.data.ChangeColor

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

  // re-render table after data/resize/scroll/sort changes
  dm.labeledDataTable.subscribe(table => vm.onDataChanged.onNext(table))

  // start streaming
  dm.windowMutations.onNext(new RefreshWindow())

  stage = new PrimaryStage() {
    title = "Scala Excel"
    scene = new Scene(root, 800, 600) {
      stylesheets add "MainStyle.css"
    }
  }

}
