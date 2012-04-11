package org.asn1gen.runtime.java;

public class AsnReal implements AsnType {
  public static AsnReal EMPTY = new AsnReal(0.0);
  
  public final double value;
  
  public AsnReal(final double value) {
    this.value = value;
  }
}
