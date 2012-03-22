package org.asn1gen.runtime.java;

import moo.model.ASNEXAMPLES.Book;

public class Person {
  public static final Person EMPTY = new Person(
      org.asn1gen.runtime.java.AsnUtf8String.EMPTY,
      org.asn1gen.runtime.java.AsnUtf8String.EMPTY);

  public final org.asn1gen.runtime.java.AsnUtf8String firstName;
  public final org.asn1gen.runtime.java.AsnUtf8String lastName;

  public Person(
      final org.asn1gen.runtime.java.AsnUtf8String firstName,
      final org.asn1gen.runtime.java.AsnUtf8String lastName) {
    this.firstName = firstName;
    this.lastName = lastName;
  }

  public boolean equals(final Person that) {
    assert that != null;
    /*GenJava.scala:215*/
    if (!this.firstName.equals(that.firstName)) {
      return false;
    }

    if (!this.lastName.equals(that.lastName)) {
      return false;
    }

    return true;
  }

  @Override
  public boolean equals(final Object that) {
    if (that instanceof Book) {
      return this.equals((Book)that);
    }

    return true;
  }

  @Override
  public int hashCode() {
    return (0
      ^ this.firstName.hashCode()
      ^ this.lastName.hashCode());
  }
  
  public static BerWriter encode(final Person person) {
    final BerWriter data = BerWriter.EMPTY
        .then(BerEncoder.encode(person.firstName))
        .then(BerEncoder.encode(person.lastName));
    return BerWriter.EMPTY.ibyte(0x30).then(data);
  }
}
