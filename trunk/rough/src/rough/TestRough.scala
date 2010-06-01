import moo.ASNEXAMPLES._

import org.asn1gen.runtime._
import org.asn1gen.runtime.Extras._
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
        .isInPrint { _ => true }

      val book2 = book1
        .isbn { _ => "987654321" }
        .title { _ => "Real World Scala" }
        .cover { _ => BookCover.paperBack }

      val book3 = defaultBook
        .isbn { _ => "987654321" }
        .title { _ => "Real World Scala" }
        .cover { _ => hardCover }

      val bookPrice1 =
        BookPrice
          .isbn { _ => "123456789" }
          .price { _ => 1234 }

      val bookPrice2 =
        BookPrice
          .isbn { _ => "987654321" }
          .price { _ => 4321 }
          
      val books = Books(
          book1,
          book2,
          Book
	        .isbn { _ => "1010101010" }
	        .title { _ => "The Art of Functional Programming" }
	        .author { _ => "Someone else" }
        )
        
      val journals = Journals(
          Journal
            .title { _ => "Monologues of a mad man" }
            .edition { _ => "July 2009" }
          )

      val items = Items(
        (books.items.map { Item_book(_): Item.Book }) :::
        (journals.items.map { Item_journal(_): Item.Journal })
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
        out.print("val book3 = ")
        SimplePrinter.print(out, book3)
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
        out.println()
        out.println()
        out.print("val defaultInteger = ")
        SimplePrinter.print(out, defaultInteger)
        out.println()
        out.println()
        out.print("val defaultBooleanTrue = ")
        SimplePrinter.print(out, defaultBooleanTrue)
        out.println()
        out.println()
        out.print("val defaultBooleanFalse = ")
        SimplePrinter.print(out, defaultBooleanFalse)
        out.println()
        out.println()
        out.print("val defaultOctetString = ")
        SimplePrinter.print(out, defaultOctetString)
      }
    }
  }
}
