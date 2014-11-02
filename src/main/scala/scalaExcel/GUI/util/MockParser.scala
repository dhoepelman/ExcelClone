package scalaExcel.GUI.util

object MockParser {
  def parse(expr: String): (List[(Int, Int)], (List[Any]) => Any) = {
    //TODO actual parsing
    val refList = if (expr == "Cell22") List((0, 0), (1, 0), (0, 1)) else List()
    (refList, {
      (values: List[Any]) => {
        if (values == List()) expr else values.mkString("+")
      }
    })
  }
}
