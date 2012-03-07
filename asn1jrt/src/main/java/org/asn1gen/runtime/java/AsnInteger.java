package org.asn1gen.runtime.java;

public class AsnInteger implements AsnType {
  public static final AsnInteger EMPTY = new AsnInteger(0);
  
  public final long value;
  
  public AsnInteger(final long value) {
    this.value = value;
  }
}
