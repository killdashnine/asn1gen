package rough

import org.asn1gen.runtime._
import org.asn1gen.runtime.Extras._
import moo.ASNEXAMPLES._
import org.asn1gen.runtime.printing.SimplePrinter
import org.asn1gen.extra.Extras._

object TestRough2 {
  def main(args: Array[String]): Unit = {
    val book1 = Book
      .author { _ => "Bjarne Stroustrup" }
      .cover { _ => BookCover.paperBack }
      .isbn { _ => "123456789" }
      .isInPrint { _ => true }
      .title { _ => "Scala Programming" }
    
    val book2 = Book
      .author { _ => "Bjarne Stroustrup" }
      .cover { _ => BookCover.paperBack }
      .isbn { _ => "987654321" }
      .isInPrint { _ => true }
      .title { _ => "Real World Scala" }
    
    val book3 = Book
      .author { _ => "default author" }
      .cover { _ => BookCover.hardCover }
      .isbn { _ => "987654321" }
      .isInPrint { _ => true }
      .title { _ => "Real World Scala" }
    
    val bookPrice1 = BookPrice
      .isbn { _ => "123456789" }
      .price { _ => 1234 }
    
    val bookPrice2 = BookPrice
      .isbn { _ => "987654321" }
      .price { _ => 4321 }
    
    val books = Books(
      Book
        .author { _ => "Bjarne Stroustrup" }
        .cover { _ => BookCover.paperBack }
        .isbn { _ => "123456789" }
        .isInPrint { _ => true }
        .title { _ => "Scala Programming" },
      Book
        .author { _ => "Bjarne Stroustrup" }
        .cover { _ => BookCover.paperBack }
        .isbn { _ => "987654321" }
        .isInPrint { _ => true }
        .title { _ => "Real World Scala" },
      Book
        .author { _ => "Someone else" }
        .cover { _ => BookCover.paperBack }
        .isbn { _ => "1010101010" }
        .isInPrint { _ => false }
        .title { _ => "The Art of Functional Programming" }
    )
    
    val journals = Journals(
      Journal
        .title { _ => "Monologues of a mad man" }
        .edition { _ => "July 2009" }
    )
    
    val items = Items(
      Item
        .book { _ => Book
          .author { _ => "Bjarne Stroustrup" }
          .cover { _ => BookCover.paperBack }
          .isbn { _ => "123456789" }
          .isInPrint { _ => true }
          .title { _ => "Scala Programming" }
        },
      Item
        .book { _ => Book
          .author { _ => "Bjarne Stroustrup" }
          .cover { _ => BookCover.paperBack }
          .isbn { _ => "987654321" }
          .isInPrint { _ => true }
          .title { _ => "Real World Scala" }
        },
      Item
        .book { _ => Book
          .author { _ => "Someone else" }
          .cover { _ => BookCover.paperBack }
          .isbn { _ => "1010101010" }
          .isInPrint { _ => false }
          .title { _ => "The Art of Functional Programming" }
        },
      Item
        .journal { _ => Journal
          .title { _ => "Monologues of a mad man" }
          .edition { _ => "July 2009" }
        }
    )
    
    val defaultInteger = 123
    
    val defaultBooleanTrue = true
    
    val defaultBooleanFalse = false
    
    val defaultOctetString = "Hello world"
  
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
