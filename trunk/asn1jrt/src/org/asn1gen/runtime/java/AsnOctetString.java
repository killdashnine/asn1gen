package org.asn1gen.runtime.java;

public class AsnOctetString {
  public static final AsnOctetString EMPTY = new AsnOctetString("");
  
  public final String value;
  
  public AsnOctetString(final String value) {
    this.value = value;
  }
}
