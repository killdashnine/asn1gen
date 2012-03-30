package org.asn1gen.runtime.java;

public enum TagClass {
  UNIVERSAL(0),
  APPLICATION(1),
  CONTEXT(2),
  PRIVATE(3);
  
  public final int value;
  
  TagClass(final int value) {
    this.value = value;
  }
  
  public static TagClass fromTagByte(final int value) {
    switch ((value >> 6) & 0x3) {
    case 0: return UNIVERSAL;
    case 1: return APPLICATION;
    case 2: return CONTEXT;
    case 3: return PRIVATE;
    default: throw new RuntimeException("invalid value");
    }
  }
}
