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
        .isbn { _ => AsnOctetString("123456789") }
        .title { _ => AsnOctetString("Scala Programming") }
        .author { _ => AsnOctetString("Bjarne Stroustrup") }

      val book2 = book1
        .isbn { _ => AsnOctetString("987654321") }
        .title { _ => AsnOctetString("Real World Scala") }

      val bookPrice1 =
        BookPrice
          .isbn { _ => AsnOctetString("123456789") }
          .price { _ => 1234 }

      val bookPrice2 =
        BookPrice
          .isbn { _ => AsnOctetString("987654321") }
          .price { _ => 4321 }
          
      val books = Books(
          book1,
          book2,
          Book
	        .isbn { _ => AsnOctetString("1010101010") }
	        .title { _ => AsnOctetString("The Art of Functional Programming") }
	        .author { _ => AsnOctetString("Someone else") }
        )
        
      val journals = Journals(
          Journal
            .title { _ => AsnOctetString("Monologues of a mad man") }
            .edition { _ => AsnOctetString("July 2009") }
          )

      val items = Items(
        books.items.map { Item_book(_) } :::
        journals.items.map { Item_journal(_) }
      )

      System.out.withIndentWriter { out =>
        out.print("val book1 = ")
        SimplePrinter.print(out, book1)
        out.println()
        out.println()
        out.print("val book2 = ")
        SimplePrinter.print(out, book2)
        out.println()
        out.println()
        out.print("val bookPrice1 = ")
        SimplePrinter.print(out, bookPrice1)
        out.println()
        out.println()
        out.print("val bookPrice2 = ")
        SimplePrinter.print(out, bookPrice2)
        out.println()
        out.println()
        out.print("val books = ")
        SimplePrinter.print(out, books)
        out.println()
        out.println()
        out.print("val journals = ")
        SimplePrinter.print(out, journals)
        out.println()
        out.println()
        out.print("val items = ")
        SimplePrinter.print(out, items)
      }
    }
  }
}
