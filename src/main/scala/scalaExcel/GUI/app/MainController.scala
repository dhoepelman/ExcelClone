package scalaExcel.GUI.app

import java.net.URL
import java.util
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}
import javafx.{event => jfxe}
import javafx.{fxml => jfxf}
import scalafx.collections.ObservableBuffer
import scalafx.scene.layout.AnchorPane
import scalafx.scene.control.TableView
import scalafx.beans.property.ObjectProperty

class MainController extends jfxf.Initializable {

  @jfxf.FXML
  private var redRegion: jfxsl.Region = _

  @jfxf.FXML
  private var tableContainerDelegate: jfxsl.AnchorPane = _
  private var tableContainer: AnchorPane = _

  @jfxf.FXML
  private var formulaEditor: javafx.scene.control.TextField = _

  @jfxf.FXML
  private def makeResizable(event: jfxe.ActionEvent) {
    //make region resizable
  }

  def initialize(url: URL, rb: util.ResourceBundle) {
    tableContainer = new AnchorPane(tableContainerDelegate)
    val table = SheetBuilder.build(
      //TODO actual data
      List("A", "B"),
      List(100, 200),
      SheetBuilder.getPlainData(List(List("Cell11", "Cell12"), List("Cell21", "Cell22")))
    )
    AnchorPane.setAnchors(table, 0, 0, 0, 0)
    tableContainer.content = List(table)

    //change a cell programatically
    table.getItems.get(0).get(0).value = new SheetCell("bla", null, null)
    table.getSelectionModel.setCellSelectionEnabled(true)

    assert(formulaEditor != null)
    val editor = formulaEditor
    val b = new ObservableBuffer(table.getSelectionModel.getSelectedCells)
    b.onChange(
      (source, changes) => {
        source.map(x => (x.getRow, x.getColumn))                  // get coords
              .map(x => table.getItems.get(x._1).get(x._2).value) // get value
              .take(1)                                            // take only one
              .map(x => editor.setText(x.toString))               // print value

      }
    )
  }
}
