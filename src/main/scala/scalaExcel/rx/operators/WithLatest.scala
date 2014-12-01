
package scalaExcel.rx.operators

import rx.lang.scala.Observable

object WithLatest {

  // TODO: Replace this with RxJava's operator if/when it gets available: https://github.com/ReactiveX/RxJava/issues/405
  // Source: http://stackoverflow.com/a/27207073/572635
  implicit class RxWithLatest[B](slow: Observable[B]) {
    def withLatest[A](fast : Observable[A]): Observable[(A,B)] =
      fast.map({a => slow.publish.refCount.map({b => (a,b)})}).switch
  }

}
