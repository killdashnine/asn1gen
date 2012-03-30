package org.asn1gen.runtime.java;

public enum TagForm {
  PRIMITIVE(0),
  CONSTRUCTED(1);
  
  public final int value;
  
  TagForm(final int value) {
    this.value = value;
  }
  
  public static TagForm fromTagByte(final int value) {
    switch ((value >> 5) & 0x1) {
    case 0: return PRIMITIVE;
    case 1: return CONSTRUCTED;
    default: throw new RuntimeException("invalid value");
    }
  }
}
