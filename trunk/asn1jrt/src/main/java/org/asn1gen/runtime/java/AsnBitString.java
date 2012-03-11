package org.asn1gen.runtime.java;

public class AsnBitString implements AsnType {
  public final long value;
  public final int length;
  
  public AsnBitString(final long value, final int length) {
    this.value = value;
    this.length = length;
  }
}
