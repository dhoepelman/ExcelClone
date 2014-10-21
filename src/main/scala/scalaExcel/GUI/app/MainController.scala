package scalaExcel.GUI.app

import java.net.URL
import java.util
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}
import javafx.{event => jfxe}
import javafx.{fxml => jfxf}
import scalafx.scene.layout.AnchorPane

class MainController extends jfxf.Initializable {

  @jfxf.FXML
  private var redRegion: jfxsl.Region = _

  @jfxf.FXML
  private var tableContainerDelegate: jfxsl.AnchorPane = _
  private var tableContainer: AnchorPane = _

//  @jfxf.FXML
//  private var choiceBox: jfxsc.ChoiceBox[String] = _
//

  @jfxf.FXML
  private def makeResizable(event: jfxe.ActionEvent) {
    //make region resizable
  }

  def initialize(url: URL, rb: util.ResourceBundle) {
    tableContainer = new AnchorPane(tableContainerDelegate)

    //TODO add table with data to tableContainer
    //TODO    or use "mainTable" component and fill it

  }
}
