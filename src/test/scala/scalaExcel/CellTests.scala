
package scalaExcel

import org.junit.Test
import org.junit.Assert._

class CellTests {

  @Test def testCell {
    val c1 = Cell(TermInt(4))
    val c2 = Cell(TermInt(5))
    val c3 = Cell(TermInt(3))
    val c4 = Cell(TermAdd(TermCell(c1), TermCell(c2)))
    val c5 = Cell(TermAdd(TermCell(c4), TermCell(c3)))
    val c6 = Cell(TermAdd(TermCell(c5), TermCell(c3)))

    assertEquals(12, c5.getValue)
    assertEquals(15, c6.getValue)

    c3.setFunc(TermInt(8))

    assertEquals(17, c5.getValue)
    assertEquals(25, c6.getValue)
  }

  @Test def getLeafCells {
    val c1 = Cell(TermInt(1))
    val c2 = Cell(TermInt(2))
    val c3 = Cell(TermInt(3))
    val c4 = Cell(TermInt(4))

    val c5 = Cell(TermAdd(
      TermAdd(
        TermAdd(
          TermAdd10(TermCell(c2)),
          TermCell(c3)
        ),
        TermCell(c4)
      ),
      TermCell(c1)
    ))

    val leafs = c5.findDepCells
    assertEquals(4, leafs.length)
  }

  @Test def testAdd10 {
    val c1 = Cell(TermInt(5))
    val c2 = Cell(TermInt(1))
    val c3 = Cell(TermAdd(TermCell(c1), TermCell(c2)))

    assertEquals(6, c3.getValue)
    c1.setFunc(TermInt(7))
    assertEquals(8, c3.getValue)
    c2.setFunc(TermAdd10(TermCell(c1)))
    assertEquals(24, c3.getValue)
  }

  @Test def testMinMult {
    val c1 = Cell(TermInt(5))
    val c2 = Cell(TermInt(2))
    val c3 = Cell(TermMin(TermCell(c1), TermCell(c2)))
    val c4 = Cell(TermMult(TermCell(c2), TermCell(c3)))

    assertEquals(6, c4.getValue)
  }

  @Test def nestedTerms {
    val c1 = Cell(TermInt(1))
    val c2 = Cell(TermInt(2))
    val c3 = Cell(TermInt(3))
    val c4 = Cell(TermInt(4))

    val c5 = Cell(TermAdd(
      TermAdd(
        TermAdd(
          TermAdd10(TermCell(c2)),
          TermCell(c3)
        ),
        TermCell(c4)
      ),
      TermCell(c1)
    ))

    assertEquals(20, c5.getValue)
    c2.setFunc(TermInt(5))
    assertEquals(23, c5.getValue)
  }

}
