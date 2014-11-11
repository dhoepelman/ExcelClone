package scalaExcel.GUI.util

import scalaExcel.formula.Parser;
import scalaExcel.formula.Evaluator.{eval, Ctx}
import scalaExcel.formula.{ACell, VDouble}

// TODO this should be renamed, into CellEvaluator or something...
object MockParser {

  val p = new Parser

  def parse(formula: String): (List[(Int, Int)], (List[Any]) => Any) = {

    val expr = p.parse(formula) match {
      case p.Success (t, _) => t
      // this should show some nice dialog that the user did something wrong
      case p.NoSuccess(msg, _) => throw new IllegalArgumentException(s"Unable to parse $formula: $msg")
    }

    // refList should come from the AST
    val regex = """ref(\d+)""".r
    val stringRefs = List() ++ regex.findAllIn(formula)
    val refList = List() ++ stringRefs
      .map(x => List() ++ """\d""".r.findAllIn(x).take(2).map(x => x.toInt))
      .map(x => (x.head, x.last))

    // return a tuple, with references and a function that evaluates the
    // expression when the values are known
    (refList, (values: List[Any]) => {

      //Fake formula evaluation which only replaces refxx
//      values match {
//        case List() => formula
//        case l => stringRefs.zip(values).foldLeft(formula) {
//          case (z, (s, null)) => z.replaceAll(s, "")
//          case (z, (s, r)) => z.replaceAll(s, r.toString)
//        }
//      }

      // values should become the Context eventually
      val ctx: Ctx = Map(
        ACell("A", 1) -> VDouble(5)
      )
      eval(ctx, expr)
    })
  }
}
