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
                   val onColumns: Boolean,
                   val indexLabelList: List[(Int, String)])
  extends Stage {

  type DataNumNum = javafx.scene.chart.XYChart.Data[Number, Number]

  title = "Chart Viewer"

  // Create chart
  val xAxis = new NumberAxis
  val yAxis = new NumberAxis
  val lineChart = LineChart(xAxis, yAxis)
  lineChart.title = "Chart"

  // Create window
  scene = new Scene(800, 600) {
    root = lineChart
  }

  val series = indexLabelList
    .map({case (_, label) =>
      val s = new Series[Number, Number]
      s.name = (if (onColumns) "Column " else "Row ") + label
      s
    })
  series.foreach(s => lineChart.getData.add(s))

  // TODO get series labels from first row/column?

  // Project every change from the model to the charts
  sheets.map(sheetToData(_,indexLabelList.map({case (index, _) => index})))
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
    series.data.getValue.clear()
    data.foreach(series.data.getValue.add)
  }


  /**
   * Creates a list of series out of a sheet's columns
   *
   * @param sheet A sheet out of which to construct graphs
   * @param indexList The indexes of the collections to be used in creating the graphs
   * @return
   */
  def sheetToData(sheet: Sheet, indexList: List[Int]) : List[IndexedSeq[DataNumNum]] = {
    println("Calculating new graph")
    indexList.map(index => {
      val values =
        if (onColumns)
          (0 to sheet.rows)
            .map(r => sheet.getValue((index, r)))
        else
          (0 to sheet.cols)
            .map(c => sheet.getValue((c, index)))
      values.zipWithIndex
        .filter({
        // Completely ignore non-numbers
        case (VDouble(_), _) => true
        case _ => false
      })
        .map {
        case (VDouble(v), i) => (v, i)
        case _ => (0.0, 0) // Never occurs
      }
        .map(t => (t._2, t._1)) // Swap to put index on the x axis
        .map { case (x, y) => new javafx.scene.chart.XYChart.Data[Number, Number](x, y)}
    }).toList
  }

}
