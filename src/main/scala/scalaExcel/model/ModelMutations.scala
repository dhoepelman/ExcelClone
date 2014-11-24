package scalaExcel.model

import scalafx.scene.paint.Color

/**
 * Types of inputs to the data model
 */
abstract class ModelMutations
case class Refresh() extends ModelMutations
case class SetFormula(x: Int, y: Int, f: String) extends ModelMutations
//case class SetColor(x: Int, y: Int, c: Color) extends ModelMutations
case class SetStyle(x: Int, y: Int, s: Styles) extends ModelMutations
