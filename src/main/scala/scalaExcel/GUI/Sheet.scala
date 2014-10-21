import javafx.scene.control.Label
import javafx.scene.layout.{GridPane, StackPane}
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle

class Cell (width : Int = 80, height : Int = 25, indexRow : Int = 0 , indexColumn : Int = 0)
  extends StackPane
{
  val border = new Rectangle(width+2, height+2, Color.DARKGREY)
  val fill = new Rectangle(width, height, Color.WHITE)
  val label = new Label
  label.setText("" + indexRow + "." + indexColumn)

  getChildren.addAll(border, fill, label)
}


class SheetPage (rows : Int = 5, columns : Int = 5, rowOffset : Int, columnOffset : Int)
  extends GridPane
{
  for ( c <- 0 until columns)
  {
    for ( r <- 0 until rows )
    {
      this.add(new Cell(indexRow = r + rowOffset, indexColumn = c + columnOffset),
        c ,
        r)
    }
  }
}


class Sheet (rows : Int = 3, columns : Int = 3)
  extends GridPane
{
  private val ssz = 5

  for ( c <- 0 until columns)
  {
    for ( r <- 0 until rows )
    {
      this.add(
        new SheetPage(
          rows = ssz,
          columns = ssz,
          rowOffset = ssz*r,
          columnOffset = ssz*c
        ),
        c,
        r)
    }
  }
}


class PaginatedSheet
{

}