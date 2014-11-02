package scalaExcel.GUI.view

import java.net.URL
import java.util
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}
import javafx.{event => jfxe, fxml => jfxf}
import scalafx.collections.ObservableBuffer
import scalafx.scene.layout.AnchorPane
import rx.lang.{scala => rx}
import scalaExcel.GUI.controller.Mediator


class ViewManager extends jfxf.Initializable {

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
    val table = Mediator.getTableView
    AnchorPane.setAnchors(table, 0, 0, 0, 0)
    tableContainer.content = List(table)

    Mediator.changeCellExpr((0,0), "new")

    //change a cell programatically and test subscription

    //    val cell00 = table.getItems.get(0).get(0)
    //    val cell01 = table.getItems.get(0).get(1)
    //    val cell10 = table.getItems.get(1).get(0)
    //    val cell11 = table.getItems.get(1).get(1)
    ////    val sub = cell00.subject.subscribe(cell01.refresher)
    //    val combi = cell00.subject.combineLatest(cell10.subject)
    //    val ha = (bla:((Int, Int, SheetCell), (Int, Int, SheetCell))) => println(bla)
    //    combi.subscribe(ha)

    //    table.getItems.get(0).get(0).value = new SheetCell("bla", null, null)
    //        sub.unsubscribe()
    //        table.getItems.get(0).get(0).value = new SheetCell("huhuuu", null, null)



    table.getSelectionModel.setCellSelectionEnabled(true)

//    assert(formulaEditor != null)
//    val editor = new scalafx.scene.control.TextField(formulaEditor)
//
//    // Display selected cell onto the formula editor
//    val selectedCells = new ObservableBuffer(table.getSelectionModel.getSelectedCells)
//    selectedCells.onChange(
//      (source, changes) => {
//        source.map(x => (x.getRow, x.getColumn)) // get coords
//          .map(x => Mediator.getCell(x)) // get value
//          .take(1) // take only one
//          .map(x => editor.setText(x.toString)) // print value
//      }
//    )
//
//    // Edits on the formula editor are reflected on the cells
//    formulaEditor.setOnAction(new javafx.event.EventHandler[javafx.event.ActionEvent] {
//      override def handle(e: javafx.event.ActionEvent): Unit = {
//        val editorValue = editor.getText
//        selectedCells.map(x => (x.getRow, x.getColumn))
//          .map(x => Mediator.changeCellExpr(x, editorValue))
//      }
//    })
  }

}
