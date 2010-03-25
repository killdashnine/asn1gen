import moo.ASNEXAMPLES._

import org.asn1gen.runtime._
import org.asn1gen.runtime.printing.SimplePrinter
import org.asn1gen.extra.Extras._
import org.asn1gen.io.IndentWriter
import java.io.PrintWriter

package rough {
  object TestRough {
    def main(args: Array[String]): Unit = {
      val book1 = Book
        .isbn { _ => "123456789" }
        .title { _ => "Scala Programming" }
        .author { _ => "Bjarne Stroustrup" }

      val book2 = book1
        .isbn { _ => "987654321" }
        .title { _ => "Real World Scala" }

      val bookPrices = BookPrices(
        BookPrice
          .isbn { _ => "123456789" }
          .price { _ => 1234 },
        BookPrice
          .isbn { _ => "123456789" }
      )
      
      /*
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
        } }*/
      
      System.out.withIndentWriter { out =>
        out.print("book1 = ")
        SimplePrinter.print(out, book1)
        out.println()
        out.println()
        out.print("book2 = ")
        SimplePrinter.print(out, book2)
        out.println()
        out.println()
        out.print("bookPrices = ")
        SimplePrinter.print(out, bookPrices)
      }
    }
  }
}
