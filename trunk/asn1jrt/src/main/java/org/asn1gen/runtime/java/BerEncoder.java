package org.asn1gen.runtime.java;


public class BerEncoder {
  private static final byte[] TRUE_BYTES = new byte[] { 1, 1, -1 };
  private static final byte[] FALSE_BYTES = new byte[] { 1, 1, 0 };
  private static final byte[] NULL_BYTES = new byte[] { 5, 0, 0 };
  
  public BerEncoder() {
  }
  
  public BerWriter encode(final AsnBoolean value) {
    if (value.value) {
      return BerWriter.writeBytes(TRUE_BYTES);
    } else {
      return BerWriter.writeBytes(FALSE_BYTES);
    }
  }
  
  public BerWriter encode(final AsnNull value) {
    return BerWriter.writeBytes(NULL_BYTES);
  }
  
  public BerWriter encode(final AsnInteger value) {
    final BerWriter dataWriter = BerWriter.writeVariableInteger(value.value);
    return BerWriter.write(
        BerWriter.writeUnsignedByte(2),
        // TODO: Use proper length
        BerWriter.writeUnsignedByte(dataWriter.bytes),
        dataWriter);
  }
}
