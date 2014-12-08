package scalaExcel.model

import org.junit.Assert._
import org.junit._

class UndoRedoTests {
  val zeroToFive = new UndoRedo(0).next(1).next(2).next(3).next(4).next(5)
  val zeroToFiveUndoToZero = zeroToFive.undo().undo().undo().undo().undo()

  @Test def undoTest() : Unit = {
    List(4,3,2,1,0).foldLeft(zeroToFive)((undoRedo, expected) => {
      val res = undoRedo.undo()
      assertEquals(expected, res.current)
      res
    })
  }

  @Test def redoTest() : Unit = {
    List(1,2,3,4).foldLeft(zeroToFiveUndoToZero)((undoRedo, expected) => {
      val res = undoRedo.redo()
      assertEquals(expected, res.current)
      res
    })
  }

  @Test def noRedoAfterAnotherChange() : Unit = {
    assertEquals(100,
      zeroToFiveUndoToZero
      .next(100)
      .redo() // This should be a No-op
      .current
    )
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
