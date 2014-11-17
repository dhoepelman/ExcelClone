package scalaExcel.model

/**
 * Created by David on 17-11-2014.
 */
// Types of inputs to the data model
abstract class SheetMutations
case class SetCell(x: Int, y: Int, f: String) extends SheetMutations