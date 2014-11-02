package scalaExcel.GUI.view

import scalafx.Includes._
import scalafx.application.JFXApp
import javafx.{scene => jfxs}
import javafx.{fxml => jfxf}
import javafx.{scene => jfxs}
import java.io.IOException
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import rx.lang.{scala => rx}
import scalaExcel.GUI.controller.Mediator

object MainView extends JFXApp {

  Mediator.initialize()

  val resource = getClass.getResource("/MainContainer.fxml")

  if (resource == null) {
    throw new IOException("Cannot load resource: MainContainer.fxml")
  }

  val loader = new jfxf.FXMLLoader(resource)
  val root = loader.load[jfxs.Parent]
  val controller = loader.getController[ViewManager]

  Mediator.registerController(controller)

  stage = new PrimaryStage() {
    title = "Scala Excel"
    scene = new Scene(root, 800, 600) {
      stylesheets add "MainStyle.css"
    }
  }

}