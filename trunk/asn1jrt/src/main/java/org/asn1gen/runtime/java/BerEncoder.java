package org.asn1gen.runtime.java;

public class BerEncoder {
  public BerEncoder() {
  }
  
  public BerWriter encode(final BerWriter preceeding, final AsnBoolean value) {
    if (value.value) {
      return preceeding.ibyte(0x01).ibyte(0x01).ibyte(0xff);
    } else {
      return preceeding.ibyte(0x01).ibyte(0x01).ibyte(0x00);
    }
  }
  
  public BerWriter encode(final BerWriter preceeding, final AsnNull value) {
    return preceeding.ibyte(0x05).ibyte(0x00).ibyte(0x00);
  }

  public BerWriter tagIdTail(final BerWriter preceeding, final long tagId) {
    return tagIdTail(preceeding, tagId, 0x00);
  }
  
  public BerWriter tagIdTail(final BerWriter preceeding, final long tagId, final int terminus) {
    final long excessTagId = tagId >>> 7;
    final int capturedTagId = (int)tagId & 0x7f;
    if (excessTagId != 0) {
      return tagIdTail(preceeding, excessTagId, 0x80).ibyte(capturedTagId | terminus);
    } else {
      return preceeding.lbyte(capturedTagId | terminus);
    }
  }

  public BerWriter length(final BerWriter preceeding, final long value) {
    if (value < 0) {
      throw new IllegalArgumentException();
    }
    
    if (value <= 127) {
      return preceeding.lbyte(value);
    }
    
    final BerWriter tail = i8sig(BerWriter.EMPTY, value);
    
    return preceeding.ibyte(tail.length | 0x80).then(tail);
  }
  
  public BerWriter i8sig(final BerWriter preceeding, final long value) {
    final long excessValue = value >>> 8;
    final long capturedValue = value & 0xff;
    
    if (excessValue > 0) {
      return i8sig(preceeding, excessValue).lbyte(capturedValue);
    } else {
      return preceeding.lbyte(capturedValue);
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
    
    if (tagId <= 30) {
      value |= (int)tagId;
      
      return preceeding.ibyte(value);
    } else {
      value |= 0x1f; // 0001 1111
      
      return tagIdTail(preceeding.ibyte(value), tagId);
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
  
  public BerWriter d(final double value) {
    if (value == 0) {
      return BerWriter.EMPTY.ibyte(9).ibyte(0);
    }
    
    if (value == Double.POSITIVE_INFINITY) {
      return BerWriter.EMPTY.ibyte(9).ibyte(1).ibyte(0x40);
    }
    
    if (value == Double.NEGATIVE_INFINITY) {
      return BerWriter.EMPTY.ibyte(9).ibyte(1).ibyte(0x41);
    }
    
    final long rawValue = java.lang.Double.doubleToRawLongBits(value);
    final long sign = (rawValue >> 63) & 0x1;
    final int scale = 0;
    final int base = 0; // binary
    final BerWriter encodedMantissa = i8sig(BerWriter.EMPTY, rawValue & 0x000fffffffffffffL);
    final BerWriter encodedExponent = i8sig(BerWriter.EMPTY, (rawValue >> 52) & 0x7ff);
    final BerWriter encodedDescriptor = BerWriter.EMPTY.lbyte(
        (0x80 | (sign << 6) | (base << 4) | (scale << 2) | (encodedExponent.length & 0x3)));
    final BerWriter realData = encodedDescriptor.then(encodedExponent).then(encodedMantissa);
    
    return BerWriter.EMPTY.ibyte(9).ibyte(realData.length + 1).then(realData);
  }
  
  public BerWriter encode(final AsnReal value) {
    return d(value.value);
  }
}
