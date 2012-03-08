package org.asn1gen.runtime.java;


public class BerEncoder {
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

  public BerWriter complexTagId(final BerWriter preceeding, final long tagId) {
    return complexTagId(preceeding, tagId, 0x00);
  }
  
  public BerWriter complexTagId(final BerWriter preceeding, final long tagId, final int terminus) {
    final long excessTagId = tagId >>> 7;
    final int capturedTagId = (int)tagId & 0x7f;
    if (excessTagId != 0) {
      return complexTagId(preceeding, excessTagId, 0x80).ibyte(capturedTagId | terminus);
    } else {
      return preceeding.lbyte(capturedTagId | terminus);
    }
  }
  
  public BerWriter tag(final BerWriter preceeding, final AsnClass clazz, final AsnForm form, final long tagId) {
    if (tagId < 0) {
      throw new IllegalArgumentException();
    }
    
    int value = 0;

    switch (clazz) {
    case UNIVERSAL:
      value |= 0x00; // 0000 0000
      break;
    case APPLICATION:
      value |= 0x40; // 0100 0000
      break;
    case CONTEXT_SPECIFIC:
      value |= 0x80; // 1000 0000
      break;
    case PRIVATE:
      value |= 0xc0; // 1100 0000
      break;
    }
    
    switch (form) {
    case PRIMITIVE:
      value |= 0x00; // 0000 0000
      break;
    case CONSTRUCTED:
      value |= 0x20; // 0010 0000
      break;
    }
    
    if (tagId <= 0 && tagId <= 30) {
      value |= (int)tagId;
      
      return preceeding.ibyte(value);
    } else {
      value |= 0x1f; // 0001 1111
      
      return complexTagId(preceeding.ibyte(value), tagId);
    }
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
