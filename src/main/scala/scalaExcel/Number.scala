
package scalaExcel

case class Number(a: Int) {

  def add(b: Int) = Number(a + b)
  def min(b: Int) = Number(a - b)

}
