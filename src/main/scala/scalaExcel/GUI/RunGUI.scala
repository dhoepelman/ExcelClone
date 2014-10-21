/*
    Run GUI from here
 */

package scalaExcel.GUI

import javafx.application.Application
import javafx.beans.property._
import javafx.collections.FXCollections
import javafx.scene.Scene
import javafx.scene.control.cell.PropertyValueFactory
import javafx.scene.control.{TableCell, TableColumn, TableView}
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javafx.stage.Stage
import javafx.util.Callback

class Person (fName : String, lName : String, eMail : String){
  val firstName = new SimpleStringProperty(fName)
  val lastName = new SimpleStringProperty(lName)
  val email = new SimpleStringProperty(eMail)

  def getFirstName() : String = {
    return firstName.get();
  }
  def setFirstName(fName : String) : Unit = {
    firstName.set(fName);
  }

  def getLastName() : String = {
    return lastName.get();
  }
  def setLastName(fName : String) : Unit = {
    lastName.set(fName);
  }

  def getEmail() : String = {
    return email.get();
  }
  def setEmail(fName : String) : Unit = {
    email.set(fName);
  }
}


class Test extends Application {
  println("Test()")

  val people = FXCollections.observableArrayList[Person](
    new Person("Jacob", "Smith", "jacob.smith@example.com"),
    new Person("Isabella", "Johnson", "isabella.johnson@example.com"),
    new Person("Ethan", "Williams", "ethan.williams@example.com"),
    new Person("Emma", "Jones", "emma.jones@example.com"),
    new Person("Michael", "Brown", "michael.brown@example.com")
  )

  override def start(primaryStage: Stage) {
    primaryStage.setTitle("Sup!")

    val root = new StackPane();
    val s = new Scene(root, 700, 300, Color.IVORY);

    val table = new TableView[Person]
    table.prefWidthProperty().bind(root.prefHeightProperty())
    table.setEditable(true);

    val firstNameCol = new TableColumn[Person,String]("First Name");
    val lastNameCol = new TableColumn[Person,String]("Last Name");
    val emailCol = new TableColumn[Person,String]("Email");
    firstNameCol.setMinWidth(50)
    lastNameCol.setMinWidth(50)
    emailCol.setMinWidth(50)

    emailCol.setCellFactory(new Callback[TableColumn[Person, String], TableCell[Person, String]]
    {
      def call(personStringTableColumn : TableColumn[Person, String]) : TableCell[Person, String] =
      {
        new CustomCell
      }
    });

    firstNameCol.setCellValueFactory(
      new PropertyValueFactory[Person,String]("firstName")
    )
    lastNameCol.setCellValueFactory(
      new PropertyValueFactory[Person,String]("lastName")
    )
    emailCol.setCellValueFactory(
      new PropertyValueFactory[Person,String]("email")
    )

    table.getColumns.addAll(firstNameCol, lastNameCol, emailCol);
    table.setItems(people)


    root.getChildren.addAll(table)

    primaryStage.setScene(s)
    primaryStage.show()
  }
}


class CustomCell
  extends TableCell[Person, String]
{
  override def updateItem(item : String, empty : Boolean): Unit = {
    if ( !empty ) {
      if ( item.contains("@") ) {
        setText(item)
        getStyleClass.add("dangerzone")
        setStyle("{-fx-background-color: azure; -fx-text-fill: indigo;} .selected {-fx-text-fill: red;}")
      }
    }
  }
}


object Test {
  def main(args: Array[String]) {
    Application.launch(classOf[Test], args: _*)
  }
}