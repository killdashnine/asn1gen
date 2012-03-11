package org.asn1gen.runtime.java;

public class AsnBitString implements AsnType {
  public final long value;
  public final int length;
  
  public AsnBitString(final long value, final int length) {
    if (length < 0) {
      throw new IllegalArgumentException();
    }
    
    if (length > 64) {
      throw new IllegalArgumentException("Currently doesn't support bit strings longer than 64-bits");
    }
    
    this.value = value;
    this.length = length;
  }
}
