package scalaExcel.model.reactive

import rx.lang.scala.Observable

import scalaExcel.model.ModelMutations

class Model(protected val sheetMutations : Observable[ModelMutations]) extends scalaExcel.model.Model {

}
