package org.asn1gen.runtime.java;

import static org.asn1gen.runtime.java.AsnDataToBerEncoder.encodeData;

public class AsnToBerEncoder {
  public static final BerWriter EMPTY = BerWriter.EMPTY;
  public static final BerWriter ASN_TRUE = BerWriter.EMPTY.ibyte(0x01).ibyte(0x01).ibyte(0xff);
  public static final BerWriter ASN_FALSE = BerWriter.EMPTY.ibyte(0x01).ibyte(0x01).ibyte(0x00);
  public static final BerWriter ASN_NULL = BerWriter.EMPTY.ibyte(0x05).ibyte(0x00);
  
  public static BerWriter encode(final AsnBoolean value) {
    return encode(value.value);
  }
  
  public static BerWriter encode(final boolean value) {
    return value ? ASN_TRUE : ASN_FALSE;
  }
  
  public static BerWriter encode(final AsnNull value) {
    return ASN_NULL;
  }

  public static BerWriter encode(final long value) {
    final BerWriter dataWriter = encodeData(value);
    // TODO: Use proper length
    return BerWriter.EMPTY.ibyte(2).ibyte(dataWriter.length).then(dataWriter);
  }
  
  public static BerWriter encode(final AsnInteger value) {
    final BerWriter dataWriter = encodeData(value);
    // TODO: Use proper length
    return BerWriter.EMPTY.ibyte(2).ibyte(dataWriter.length).then(dataWriter);
  }
  
  public static BerWriter encode(final double value) {
    final BerWriter dataWriter = encodeData(value);
    // TODO: Use proper length
    return BerWriter.EMPTY.ibyte(9).ibyte(dataWriter.length).then(dataWriter);
  }
  
  public static BerWriter encode(final AsnReal value) {
    final BerWriter dataWriter = encodeData(value);
    // TODO: Use proper length
    return BerWriter.EMPTY.ibyte(9).ibyte(dataWriter.length).then(dataWriter);
  }
  
  public static BerWriter encode(final AsnBitString value) {
    final BerWriter dataWriter = encodeData(value);
    // TODO: Use proper length
    return EMPTY.ibyte(3).ibyte(dataWriter.length).then(dataWriter);
  }

  public static BerWriter encode(final AsnOctetString value) {
    final BerWriter dataWriter = encodeData(value);
    // TODO: Use proper length
    return EMPTY.ibyte(4).ibyte(dataWriter.length).then(dataWriter);
  }

  public static BerWriter encode(final AsnUtf8String value) {
    final BerWriter dataWriter = encodeData(value);
    // TODO: Use proper length
    return EMPTY.ibyte(12).ibyte(dataWriter.length).then(dataWriter);
  }
}
