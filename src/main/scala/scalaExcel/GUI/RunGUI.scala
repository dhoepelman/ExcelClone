package scalaExcel.GUI

import javafx.application.Application
import javafx.scene.control.{Label, Button}
import javafx.scene.{Scene, Group}
import javafx.scene.layout.{ColumnConstraints, GridPane, StackPane}
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle
import javafx.stage.Stage
import javafx.scene.control._

import javafx.collections._


class Test extends Application {
  println("Test()")

  override def start(primaryStage: Stage) {
    primaryStage.setTitle("Sup!")

    val root = new Group();
    val s = new Scene(root, 300, 300, Color.IVORY);

    primaryStage.setScene(s)
    primaryStage.show()
  }
}


object Test {
  def main(args: Array[String]) {
    Application.launch(classOf[Test], args: _*)
  }
}
