package org.asn1gen.runtime.java;

public class BerToAsn {
  public static final BerWriter EMPTY = BerWriter.EMPTY;
  public static final BerWriter ASN_TRUE = BerWriter.EMPTY.ibyte(0x01).ibyte(0x01).ibyte(0xff);
  public static final BerWriter ASN_FALSE = BerWriter.EMPTY.ibyte(0x01).ibyte(0x01).ibyte(0x00);
  public static final BerWriter ASN_NULL = BerWriter.EMPTY.ibyte(0x05).ibyte(0x00);
  public static final BerWriter TRUE = BerWriter.EMPTY.ibyte(0xff);
  public static final BerWriter FALSE = BerWriter.EMPTY.ibyte(0x00);
  public static final BerWriter NULL = BerWriter.EMPTY;
  
  public static AsnBoolean decode(final AsnBoolean value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnBoolean.EMPTY;
  }
  
  public static AsnNull decode(final AsnNull value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnNull.EMPTY;
  }

  public static AsnInteger decode(final AsnInteger value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnInteger.EMPTY;
  }
  
  public static AsnReal decode(final AsnReal value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnReal.EMPTY;
  }
  
  public static AsnBitString decode(final AsnBitString value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnBitString.EMPTY;
  }

  public static AsnOctetString decode(final AsnOctetString value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnOctetString.EMPTY;
  }

  public static AsnUtf8String decode(final AsnUtf8String value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnUtf8String.EMPTY;
  }

  public static AsnBoolean decodePart(final AsnBoolean value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnBoolean.EMPTY;
  }
  
  public static AsnNull decodePart(final AsnNull value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnNull.EMPTY;
  }
  
  public static AsnInteger decodePart(final AsnInteger value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnInteger.EMPTY;
  }
  
  public static AsnReal decodePart(final AsnReal value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnReal.EMPTY;
  }
  
  public static AsnBitString decodePart(final AsnBitString value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnBitString.EMPTY;
  }

  public static AsnOctetString decodePart(final AsnOctetString value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnOctetString.EMPTY;
  }

  public static AsnUtf8String decodePart(final AsnUtf8String value, final ByteArrayWindow window, final ReturnInteger consumed) {
    return AsnUtf8String.EMPTY;
  }
}
