import moo.AMPTYPES._

import org.asn1gen.runtime.printing.SimplePrinter
import org.asn1gen.extra.Extras._
import org.asn1gen.io.IndentWriter
import java.io.PrintWriter

package rough {
  object TestRough {
    def main(args: Array[String]): Unit = {
      println("Hello world")
      val secBoardId1 =
        ( AmpSecBoardId
            .secCode { _ => AmpSecurityCode("xyz") }
        )
      val secBoardId2 =
        ( AmpSecBoardId
            .secCode { _ => AmpSecurityCode("xyz") }
        )
      val secBoardId3 =
        ( AmpSecBoardId
            .secCode { _ => AmpSecurityCode("abc") }
        )
      println(secBoardId1)
      println(secBoardId1.secCode)
      println(secBoardId1.boardId)
      println(secBoardId2)
      println(secBoardId2.secCode)
      println(secBoardId2.boardId)
      println("?")
      println(secBoardId1 == secBoardId2)
      println(secBoardId1 == secBoardId3)
      println(secBoardId1._desc.name)
      println(secBoardId1._desc.children)
      secBoardId1._desc.children.foreach { case (key, value) =>
        println(key + ": " + value)
      }
      
      System.out.withIndentWriter { writer =>
        SimplePrinter.print(writer, secBoardId1)
      }
    }
  }
}
