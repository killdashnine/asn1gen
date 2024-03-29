/* This file was generated by asn1gen */

package moo.model.ASNEXAMPLES;

import org.asn1gen.runtime.java.*;

import static org.asn1gen.runtime.java.Statics.*;

@SuppressWarnings("unused")
public class BookPrices extends org.asn1gen.runtime.java.AsnList {
  public static BookPrices EMPTY = new BookPrices(org.asn1gen.runtime.java.ConsList.<Book>nil());

  public final org.asn1gen.runtime.java.ConsList<Book> items;

  public BookPrices(final org.asn1gen.runtime.java.ConsList<Book> items) {
    this.items = items;
  }

  public BookPrices withItems(final org.asn1gen.runtime.java.ConsList<Book> value) {
    return new BookPrices(value);
  }

  public boolean equals(final BookPrices that) {
    assert that != null;

    return this.items.equals(that.items);
  }

  public boolean equals(final Object that) {
    if (that instanceof BookPrices) {
      return this.equals((BookPrices)that);
    }

    return true;
  }

  @Override
  public int hashCode() {
    return this.items.hashCode();
  }
}

