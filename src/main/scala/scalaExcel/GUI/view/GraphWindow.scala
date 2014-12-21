package scalaExcel.GUI.view

import rx.lang.scala._

import scalaExcel.formula._
import scalaExcel.model.Sheet
import scalafx.scene.chart.XYChart.Series
import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis}

/**
 * Created by Chris on 15-12-2014.
 */
class GraphWindow (val sheets: Observable[Sheet],
                   val columns: List[Int])
  extends Stage {

  type DataNumNum = javafx.scene.chart.XYChart.Data[Number, Number]

  title = "Superchart of forever"

  // Defining the axes
  val xAxis = new NumberAxis
  val yAxis = new NumberAxis

  // Creating the chart
  val lineChart = LineChart(xAxis, yAxis)
  lineChart.title = "Superchart of forever"

  scene = new Scene(800, 600) {
    root = lineChart
  }

  val series = columns
    .map(column => {
      val s = new Series[Number, Number]
      s.name = "Column " + column
      s
    })
  series.foreach(s => lineChart.getData.add(s))

  // TODO get series labels from first row?

  // Project every change from the model to the charts
  sheets.map(sheetToData(_,columns))
    .map(x => x.zip(series))
    .subscribe(_.foreach{case (d, s) =>
      applyData(s,d)
    })


  /**
   * Replaces the contents of an existing series with new data.
   *
   * @param series The series to be edited
   * @param data The data to be applied
   */
  def applyData(series: Series[Number, Number],
                data: IndexedSeq[DataNumNum]) = {
    series.data.getValue.clear
    data.foreach(series.data.getValue.add(_))
  }


  /**
   * Creates a list of series out of a sheet's columns
   *
   * @param sheet A sheet out of which to construct graphs
   * @param columns The columns to be used in creating the graphs
   * @return
   */
  def sheetToData(sheet: Sheet, columns: List[Int]) : List[IndexedSeq[DataNumNum]] = {
    println("Calculating new graph")
    columns.map(column => {
      (0 to sheet.rows)
        .map(r => sheet.getValue((column, r)))
        .zipWithIndex
        .filter(_ match { // Completely ignore non-numbers
          case (VDouble(_), _) => true
          case _ => false
        })
        .map {
          case (VDouble(v), i) => (v, i)
        }
        .map(t => (t._2, t._1)) // Swap to put index on the x axis
        .map { case (x, y) => new javafx.scene.chart.XYChart.Data[Number, Number](x, y)}
    }).toList
  }

}
