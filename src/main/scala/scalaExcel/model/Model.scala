package scalaExcel.model
import rx.lang.scala.Observable

/**
 * Represents the data model of the ScalaExcel application
 */
trait Model {
  /** Stream of errors in the model */
  def errors: Observable[Exception]

  /** Stream of sheets, emits whenever an atomic change has happened to the sheet  */
  def sheet: Observable[Sheet]
}
