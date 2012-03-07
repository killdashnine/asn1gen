package org.asn1gen.runtime.java;

public class AsnBoolean implements AsnType {
  public static final AsnBoolean TRUE = new AsnBoolean(true);
  public static final AsnBoolean FALSE = new AsnBoolean(false);
  public static final AsnBoolean EMPTY = FALSE;
  
  public final boolean value;
  
  private AsnBoolean(final boolean value) {
    this.value = value;
  }
}
