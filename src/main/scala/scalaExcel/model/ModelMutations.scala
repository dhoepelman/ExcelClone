package scalaExcel.model

/**
 * Types of inputs to the data model
 */
abstract class ModelMutations
case class Refresh() extends ModelMutations
case class SetCell(x: Int, y: Int, f: String) extends ModelMutations
case class Refresh() extends ModelMutations