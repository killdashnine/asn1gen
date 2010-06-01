package rough

import org.asn1gen.runtime._
import org.asn1gen.runtime.Extras._
import moo.ASNEXAMPLES._
import org.asn1gen.runtime.printing.SimplePrinter
import org.asn1gen.extra.Extras._

object TestRough2 {
  def main(args: Array[String]): Unit = {
    val book1 = Book
      .author { _ => null }
      .cover { _ => BookCover.paperBack }
      .isbn { _ => null }
      .isInPrint { _ => true }
      .title { _ => null }
    
    val book2 = Book
      .author { _ => null }
      .cover { _ => BookCover.paperBack }
      .isbn { _ => AsnOctetString("987654321") }
      .isInPrint { _ => true }
      .title { _ => AsnOctetString("Real World Scala") }
    
    val bookPrice1 = BookPrice
      .isbn { _ => AsnOctetString("123456789") }
      .price { _ => 1234 }
    
    val bookPrice2 = BookPrice
      .isbn { _ => AsnOctetString("987654321") }
      .price { _ => 4321 }
    
    val books = Books(
      Book
        .author { _ => null }
        .cover { _ => BookCover.paperBack }
        .isbn { _ => null }
        .isInPrint { _ => true }
        .title { _ => null },
      Book
        .author { _ => null }
        .cover { _ => BookCover.paperBack }
        .isbn { _ => AsnOctetString("987654321") }
        .isInPrint { _ => true }
        .title { _ => AsnOctetString("Real World Scala") },
      Book
        .author { _ => AsnOctetString("Someone else") }
        .cover { _ => BookCover.paperBack }
        .isbn { _ => AsnOctetString("1010101010") }
        .isInPrint { _ => false }
        .title { _ => AsnOctetString("The Art of Functional Programming") }
    )
    
    val journals = Journals(
      Journal
        .title { _ => AsnOctetString("Monologues of a mad man") }
        .edition { _ => AsnOctetString("July 2009") }
    )
    
    val items = Items(
      Item
        .book { _ => Book
          .author { _ => null }
          .cover { _ => BookCover.paperBack }
          .isbn { _ => null }
          .isInPrint { _ => true }
          .title { _ => null }
        },
      Item
        .book { _ => Book
          .author { _ => null }
          .cover { _ => BookCover.paperBack }
          .isbn { _ => AsnOctetString("987654321") }
          .isInPrint { _ => true }
          .title { _ => AsnOctetString("Real World Scala") }
        },
      Item
        .book { _ => Book
          .author { _ => AsnOctetString("Someone else") }
          .cover { _ => BookCover.paperBack }
          .isbn { _ => AsnOctetString("1010101010") }
          .isInPrint { _ => false }
          .title { _ => AsnOctetString("The Art of Functional Programming") }
        },
      Item
        .journal { _ => Journal
          .title { _ => AsnOctetString("Monologues of a mad man") }
          .edition { _ => AsnOctetString("July 2009") }
        }
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
