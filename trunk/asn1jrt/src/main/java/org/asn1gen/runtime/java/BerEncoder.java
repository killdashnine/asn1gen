package org.asn1gen.runtime.java;


public class BerEncoder {
  private static final byte[] FALSE_BYTES = new byte[] { 1, 1, 0 };
  private static final byte[] NULL_BYTES = new byte[] { 5, 0, 0 };
  
  public BerEncoder() {
  }
  
  public BerWriter encode(final AsnBoolean value) {
    if (value.value) {
      return BerWriter.EMPTY.ibyte(0x01).ibyte(0x01).ibyte(0xff);
    } else {
      return BerWriter.EMPTY.ibyte(0x01).ibyte(0x01).ibyte(0x00);
    }
  }
  
  public BerWriter encode(final AsnNull value) {
    return BerWriter.EMPTY.ibyte(0x05).ibyte(0x00).ibyte(0x00);
  }
  
  public BerWriter encode(final AsnInteger value) {
    final BerWriter dataWriter = BerWriter.EMPTY.writeVariableInteger(value.value);
    return BerWriter.EMPTY
        .ibyte(2)
        // TODO: Use proper length
        .ibyte(dataWriter.length)
        .then(dataWriter);
  }
}
