package org.asn1gen.runtime.java;

public class AsnBoolean {
  public static final AsnBoolean TRUE = new AsnBoolean(true);
  public static final AsnBoolean FALSE = new AsnBoolean(false);
  
  public final boolean value;
  
  private AsnBoolean(final boolean value) {
    this.value = value;
  }
}
