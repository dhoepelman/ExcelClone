package scalaExcel.GUI.view

import rx.lang.scala._

import scalaExcel.formula.VDouble
import scalaExcel.model.Sheet
import scalafx.application.JFXApp
import scalafx.scene.chart.XYChart.Series
import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.collections.ObservableBuffer

/**
 * Created by Chris on 15-12-2014.
 */
class GraphWindow (val sheets: Observable[Sheet],
                   val columns: List[Int])
  extends Stage {

  title = "Superchart of forever"

  // Defining the axes
  val xAxis = new NumberAxis
  xAxis.label = "Number of Month"
  val yAxis = new NumberAxis

  // Creating the chart
  val lineChart = LineChart(xAxis, yAxis)
  lineChart.title = "Superchart of forever"

  scene = new Scene(800, 600) {
    root = lineChart
  }

  val series = columns
    .map(column => new Series[Number, Number])
  series.foreach(s => lineChart.getData.add(s))

  // TODO get series labels from first row?

  // Project every change from the model to the charts
  sheets.map(sheetToData)
    .map(x => x.zip(series))
    .subscribe(_.foreach{case (d, s) =>
      s.data.getValue.clear
      d.foreach(s.data.getValue.add(_))
    })
//      s.data.getValue.clear
//      d.foreach(s.data.getValue.add(_))


  def sheetToData(sheet: Sheet) = {
    println("Calculating new graph")
    columns.map(column => {
      (0 to sheet.rows)
        .map(r => sheet.getValue((column, r)))
        .map(_ match {
        case VDouble(v) => v
        case _ => 0.0
      })
        .zipWithIndex
        .map(t => (t._2, t._1)) // Swap to put index on the x axis
        .map { case (x, y) => XYChart.Data[Number, Number](x, y)}
    }).toList
  }

}
