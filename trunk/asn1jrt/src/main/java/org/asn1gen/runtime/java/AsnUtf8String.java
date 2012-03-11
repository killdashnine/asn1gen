package org.asn1gen.runtime.java;

public class AsnUtf8String implements AsnType {
  public static final AsnUtf8String EMPTY = new AsnUtf8String("");
  
  public final String value;
  
  public AsnUtf8String(final String value) {
    this.value = value;
  }
}
