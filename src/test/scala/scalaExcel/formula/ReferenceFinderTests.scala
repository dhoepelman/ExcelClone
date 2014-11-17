package scalaExcel.formula

import org.junit.Assert._
import org.junit._

import scala.collection.immutable.{Range => SeqRange}

import scalaExcel.formula.ReferenceFinder.{findRefCells, colToNum, numToCol}
import scalaExcel.formula.Values.{toVal => tv}

class ReferenceFinderTests {

  val p = new Parser

  def test(expect: List[String], f: String) = assertEquals(
    expect map (x => p parsing("=" + x) match {
      case Cell(ColRef(c, _), RowRef(r, _)) => ACell(c, r)
      case _ => throw new IllegalArgumentException("Cell should parse to a cell")
    }),
    findRefCells(p parsing(f))
  )

  @Test def findBinOp1 = test(List("A1", "A2"), "=A1+A2")
  @Test def findBinOp2 = test(List("A1", "A2", "A3"), "=A1+A2+A3")
  @Test def findBinOp3 = test(List("B1", "B3", "B4", "B5"), "=(B1+B3) + SUM(B4, B5%)")

  @Test def findRangeCells = test(List("A1", "A2", "A3"), "=A1:A3")
  @Test def findRangeCellsRow = test(List("A1", "B1", "C1"), "=A1:C1")
  @Test def findRangeCellsBlock = test(List(
      "A1", "B1", "C1",
      "A2", "B2", "C2",
      "A3", "B3", "C3"
    ), "=A1:C3")

  // test column name utils
  @Test def _colToNum1 = assertEquals(1,   colToNum("A"))
  @Test def _colToNum2 = assertEquals(27,  colToNum("AA"))
  @Test def _colToNum3 = assertEquals(32,  colToNum("AF"))
  @Test def _colToNum4 = assertEquals(703, colToNum("AAA"))

  @Test def _numToCol1 = assertEquals("A",   numToCol(1))
  @Test def _numToCol2 = assertEquals("AA",  numToCol(27))
  @Test def _numToCol3 = assertEquals("AF",  numToCol(32))
  @Test def _numToCol4 = assertEquals("AAA", numToCol(703))

  @Test def testColToNumToCol = SeqRange(1, 1000) foreach (x => {
    assertEquals(x, colToNum(numToCol(x)))
  })

}
