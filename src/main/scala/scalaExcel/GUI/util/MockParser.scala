package scalaExcel.GUI.util

import scalaExcel.formula.Parser;
import scalaExcel.formula.Evaluator.{eval, Ctx}
import scalaExcel.formula.{ACell, VDouble}

// TODO this should be renamed, into CellEvaluator or something...
object MockParser {

  val p = new Parser();

  def parse(formula: String): (List[(Int, Int)], (List[Any]) => Any) = {

    val expr = p.parse(formula) match {
      case p.Success (t, _) => t
      // this should show some nice dialog that the user did something wrong
      case p.NoSuccess(msg, _) => throw new IllegalArgumentException(s"Unable to parse $formula: $msg")
    }

    // refList should come from the AST
    val refList = if (formula == "Cell22") List((0, 0), (1, 0), (0, 1)) else List()

    // return a tuple, with references and a function that evaluates the
    // expression when the values are known
    (refList, (values: List[Any]) => {
      // values should become the Context eventually
      val ctx: Ctx = Map(
        ACell("A", 1) -> VDouble(5)
      )
      eval(ctx, expr)
    })
  }
}
