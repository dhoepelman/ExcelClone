package scalaExcel.model

import org.junit.Assert._
import org.junit._

class UndoRedoTests {
  val zeroToFive = List(1,2,3,4,5).foldLeft(new UndoRedo(0))({ case (undoRedo, i) => undoRedo.next(i)})

  @Test def undoTest() : Unit = {
    List(4,3,2,1,0).foldLeft(zeroToFive)((undoRedo, expected) => {
      var res = undoRedo.undo()
      assertEquals(expected, res.current)
      res
    })
  }

  @Test def redoTest() : Unit = {
    val withUndone = List(4,3,2,1,0).foldLeft(zeroToFive)((undoRedo, expected) => undoRedo.undo())
    List(1,2,3,4).foldLeft(withUndone)((undoRedo, expected) => {
      var res = undoRedo.redo()
      assertEquals(expected, res.current)
      res
    })
  }

  @Test def undoAndRedoTest() : Unit = {
    val withUndone = List(4,3,2)
      .foldLeft(zeroToFive)((undoRedo, expected) => undoRedo.undo())
    val withRedo = List(3,4,5)
      .foldLeft(withUndone)((undoRedo, expected) => undoRedo.redo())
    List(4,3,2).foldLeft(withRedo)((undoRedo, expected) => {
      var res = undoRedo.undo()
      assertEquals(expected, res.current)
      res
    })
  }
}
