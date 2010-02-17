import moo.AMPTYPES._
import moo.AMPORDER._

import org.asn1gen.runtime._
import org.asn1gen.runtime.printing.SimplePrinter
import org.asn1gen.extra.Extras._
import org.asn1gen.io.IndentWriter
import java.io.PrintWriter

package rough {
  object TestRough {
    def main(args: Array[String]): Unit = {
      println("Hello world")
      val orderFixedFields1 =
        AmpOrderFixedFields
        .secBoardId { _ => Some apply AmpSecBoardId
          .securityIdType { _ => Some(AsnOctetString("")) }
          .secCode { _ => AsnOctetString("") }
          .boardId { _ => Some(AsnOctetString("")) }
        }
        .isPrivate { _ => Some(AsnBoolean) }
        .externalOrderId2 { _ => Some(AsnOctetString("")) }
        .compGenOrder { _ => Some(AsnBoolean) }
      
      val orderFixedFields2 =
        orderFixedFields1
      
      System.out.withIndentWriter { writer =>
        SimplePrinter.print(writer, orderFixedFields1)
        writer.println()
        writer.println()
      }
    }
  }
}
