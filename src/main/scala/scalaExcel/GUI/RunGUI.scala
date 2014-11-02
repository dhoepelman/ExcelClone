package scalaExcel.GUI

import scalafx.Includes._
import scalafx.application.JFXApp
import javafx.{scene => jfxs}
import javafx.{fxml => jfxf}
import javafx.{scene => jfxs}
import java.io.IOException
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalaExcel.GUI.controller.Mediator
import scalaExcel.GUI.view.ViewManager

object RunGUI extends JFXApp {

  Mediator.initialize()

  val resource = getClass.getResource("/MainContainer.fxml")

  if (resource == null) {
    throw new IOException("Cannot load resource: MainContainer.fxml")
  }

  val loader = new jfxf.FXMLLoader(resource)
  val root = loader.load[jfxs.Parent]

  Mediator.controller = loader.getController[ViewManager]

  stage = new PrimaryStage() {
    title = "Scala Excel"
    scene = new Scene(root, 800, 600) {
      stylesheets add "MainStyle.css"
    }
  }

}