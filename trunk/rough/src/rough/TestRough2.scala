package rough

import org.asn1gen.runtime._
import moo.ASNEXAMPLES._

object TestRough2 {
  def main(args: Array[String]): Unit = {
    val book1 = Book
      .isbn { _ => AsnOctetString("123456789") }
      .title { _ => AsnOctetString("Scala Programming") }
      .author { _ => AsnOctetString("Bjarne Stroustrup") }
    
    val book2 = Book
      .isbn { _ => AsnOctetString("987654321") }
      .title { _ => AsnOctetString("Real World Scala") }
      .author { _ => AsnOctetString("Bjarne Stroustrup") }
    
    val bookPrice1 = BookPrice
      .isbn { _ => AsnOctetString("123456789") }
      .price { _ => 1234 }
    
    val bookPrice2 = BookPrice
      .isbn { _ => AsnOctetString("987654321") }
      .price { _ => 4321 }
    
    val books = Books(
      Book
        .isbn { _ => AsnOctetString("123456789") }
        .title { _ => AsnOctetString("Scala Programming") }
        .author { _ => AsnOctetString("Bjarne Stroustrup") },
      Book
        .isbn { _ => AsnOctetString("987654321") }
        .title { _ => AsnOctetString("Real World Scala") }
        .author { _ => AsnOctetString("Bjarne Stroustrup") },
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
      Item
        .book { _ => Book
          .isbn { _ => AsnOctetString("123456789") }
          .title { _ => AsnOctetString("Scala Programming") }
          .author { _ => AsnOctetString("Bjarne Stroustrup") }
        },
      Item
        .book { _ => Book
          .isbn { _ => AsnOctetString("987654321") }
          .title { _ => AsnOctetString("Real World Scala") }
          .author { _ => AsnOctetString("Bjarne Stroustrup") }
        },
      Item
        .book { _ => Book
          .isbn { _ => AsnOctetString("1010101010") }
          .title { _ => AsnOctetString("The Art of Functional Programming") }
          .author { _ => AsnOctetString("Someone else") }
        },
      Item
        .journal { _ => Journal
          .title { _ => AsnOctetString("Monologues of a mad man") }
          .edition { _ => AsnOctetString("July 2009") }
        }
    )
  }
}
