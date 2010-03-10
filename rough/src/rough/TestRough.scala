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
      val orderFixedFields1 =
        AmpOrderFixedFields
        .secBoardId { _ => Some apply AmpSecBoardId
          .securityIdType { _ => Some(AsnOctetString("Security ID")) }
          .secCode { _ => AsnOctetString("") }
          .boardId { _ => Some(AsnOctetString("")) }
        }
      
      val orderFixedFields2 = orderFixedFields1
        .secBoardId {
          case Some(secBoardId) => { Some apply secBoardId
            .securityIdType {
              case Some(s@AsnOctetString(value)) => {
                Some(AsnOctetString(s.string + " modified"))
              }
              case None => None
            }
            .secCode { _ => AsnOctetString("A new seccode") }
            .boardId { _ => None }
          }
          case None => None
        }
        .externalOrderId2 { _ => Some(AsnOctetString("A new external order id")) }
      
      val orderFixedFields3 = orderFixedFields1
        .secBoardId { _ map { _
          .securityIdType { _ map { s =>
            AsnOctetString(s.string + " modified")
          } }
          .secCode { _ => AsnOctetString("A new seccode") }
          .boardId { _ => None }
        } }
        .externalOrderId2 { _ => Some(AsnOctetString("A new external order id")) }
      
      val order = AmpOrder
        .order { _ => AmpOrderEntry
          .fixed { _ => orderFixedFields3
            .externalOrderId2 { _ => Some(AsnOctetString("External order id for my order.")) }
          }
          .amendable { x => x
            .`type` { _ => AmpOrderType.limit }
          }
        }
      
      val orderSS = order
        .order { x => x
          .siteSpecific { _ => Some apply AmpOrderSiteSpecificFields
            .swx { AmpOrderSWXFields
              .prevOrderId { _ => Some apply AmpOrderId
                .orderNo { _ => 123L }
              }
            }
          }
        }
      
      val prices = AmpLegPriceList(
          AmpLegPriceList_item
            .price { _ => AmpPrice
              .value { _ => 1.0 }
            },
          AmpLegPriceList_item
            .price { _ => AmpPrice
              .value { _ => 2.0 }
            }
      )
          
      val prices2 = prices
        .items { list => list.tail map { _
          .price { _
            .decimals { _ => Some(123L) }
          }
        } }
      
      System.out.withIndentWriter { writer =>
        SimplePrinter.print(writer, orderFixedFields1)
        writer.println()
        writer.println()
        SimplePrinter.print(writer, orderFixedFields2)
        writer.println()
        writer.println()
        SimplePrinter.print(writer, orderFixedFields3)
        writer.println()
        writer.println()
        SimplePrinter.print(writer, order)
        writer.println()
        writer.println()
        SimplePrinter.print(writer, orderSS)
        writer.println()
        writer.println()
        SimplePrinter.print(writer, prices)
        writer.println()
        writer.println()
        SimplePrinter.print(writer, prices2)
      }
    }
  }
}
