/* This file was generated by asn1gen */

package moo.model.ASNEXAMPLES;

import org.asn1gen.runtime.java.*;

import static org.asn1gen.runtime.java.Statics.*;

@SuppressWarnings("unused")
public class BookPrice extends org.asn1gen.runtime.java.AsnSequence {
  public static final BookPrice EMPTY = new BookPrice(
    org.asn1gen.runtime.java.AsnOctetString.EMPTY,
    org.asn1gen.runtime.java.AsnInteger.EMPTY);

  /*GenJava.scala:411*/public final org.asn1gen.runtime.java.AsnOctetString isbn;
  public final org.asn1gen.runtime.java.AsnInteger price;

  public BookPrice(
      /*GenJava.scala:1410*/final org.asn1gen.runtime.java.AsnOctetString isbn,
      final org.asn1gen.runtime.java.AsnInteger price) {
    /*GenJava.scala:1400*/this.isbn = isbn;
    this.price = price;
  }

  public final BookPrice withIsbn(final org.asn1gen.runtime.java.AsnOctetString value) {
    return new BookPrice(
      value,
      this.price);
  }

  public final BookPrice withPrice(final org.asn1gen.runtime.java.AsnInteger value) {
    return new BookPrice(
      this.isbn,
      value);
  }

  public boolean equals(final BookPrice that) {
    assert that != null;
    /*GenJava.scala:464*/
    if (!this.isbn.equals(that.isbn)) {
      return false;
    }

    if (!this.price.equals(that.price)) {
      return false;
    }

    return true;
  }

  @Override
  public boolean equals(final Object that) {
    if (that instanceof BookPrice) {
      return this.equals((BookPrice)that);
    }

    return true;
  }

  @Override
  public int hashCode() {
    return (0
      ^ this.isbn.hashCode()
      ^ this.price.hashCode());
  }
}

