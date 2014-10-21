
package scalaExcel

import org.junit.Test
import org.junit.Assert._

class ASTTest {

  val t = new AST.Node(1,
    List(new AST.Node(2), new AST.Node(3)))

  @Test def testASTNodeMap = {
    val n = t map (_ + 1)
    assertEquals(List(2, 3, 4), n flatten)
  }

}
