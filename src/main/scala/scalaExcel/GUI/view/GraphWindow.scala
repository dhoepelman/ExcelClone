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
class GraphWindow (val sheets: Observable[Sheet])
  extends Stage {
  title = "Superchart of forever"

  // Defining the axes
  val xAxis = new NumberAxis
  xAxis.label = "Number of Month"
  val yAxis = new NumberAxis

  // Creating the chart
  val lineChart = LineChart(xAxis, yAxis)
  lineChart.title = "Superchart of forever"

  val series = new Series[Number, Number]
  series.name = "Yellow"

  sheets.map(sheetToData)
    .subscribe(d => {
      series.data.getValue.clear
      d.foreach(series.data.getValue.add(_))
    })

  lineChart.getData.add(series)

  scene = new Scene(800, 600) {
    root = lineChart
  }


  def sheetToData(sheet: Sheet) = {
    println("Calculating new graph")
    (0 to sheet.rows)
      .map(r => sheet.getValue((0,r)))
      .map(_ match {
        case VDouble(v) => v
        case _ => 0.0
      })
      .zipWithIndex
      .map(t => (t._2, t._1))
      .map {case (x, y) => XYChart.Data[Number, Number](x, y)}
  }

}
