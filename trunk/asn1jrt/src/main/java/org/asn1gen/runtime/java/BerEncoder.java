package org.asn1gen.runtime.java;

public class BerEncoder {
  public static final BerWriter EMPTY = BerWriter.EMPTY;
  public static final BerWriter TRUE = BerWriter.EMPTY.ibyte(0x01).ibyte(0x01).ibyte(0xff);
  public static final BerWriter FALSE = BerWriter.EMPTY.ibyte(0x01).ibyte(0x01).ibyte(0x00);
  public static final BerWriter NULL = BerWriter.EMPTY.ibyte(0x05).ibyte(0x00);
  
  public static BerWriter encode(final AsnBoolean value) {
    return encode(value.value);
  }
  
  public static BerWriter encode(final boolean value) {
    return value ? TRUE : FALSE;
  }
  
  public static BerWriter encode(final AsnNull value) {
    return NULL;
  }

  public static BerWriter tagIdTail(final long tagId) {
    return tagIdTail(tagId, 0x00);
  }
  
  public static BerWriter tagIdTail(final long tagId, final int terminus) {
    final long excessTagId = tagId >>> 7;
    final int capturedTagId = (int)tagId & 0x7f;
    if (excessTagId != 0) {
      return tagIdTail(excessTagId, 0x80).ibyte(capturedTagId | terminus);
    } else {
      return EMPTY.lbyte(capturedTagId | terminus);
    }
  }

  public static BerWriter length(final long value) {
    if (value < 0) {
      throw new IllegalArgumentException();
    }
    
    if (value <= 127) {
      return EMPTY.lbyte(value);
    }
    
    final BerWriter tail = i8sig(BerWriter.EMPTY, value);
    
    return EMPTY.ibyte(tail.length | 0x80).then(tail);
  }
  
  public static BerWriter i8sig(final BerWriter preceeding, final long value) {
    final long excessValue = value >>> 8;
    final long capturedValue = value & 0xff;
    
    if (excessValue > 0) {
      return i8sig(preceeding, excessValue).lbyte(capturedValue);
    } else {
      return preceeding.lbyte(capturedValue);
    }
  }
  
  public static BerWriter tag(final AsnClass clazz, final AsnForm form, final long tagId) {
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
      
      return EMPTY.ibyte(value);
    } else {
      value |= 0x1f; // 0001 1111
      
      return EMPTY.ibyte(value).then(tagIdTail(tagId));
    }
  }
  
  public static BerWriter encode(final long value) {
    final BerWriter dataWriter = BerWriter.EMPTY.writeVariableInteger(value);
    return BerWriter.EMPTY
        .ibyte(2)
        // TODO: Use proper length
        .ibyte(dataWriter.length)
        .then(dataWriter);
  }
  
  public static BerWriter encode(final AsnInteger value) {
    return encode(value.value);
  }
  
  private static BerWriter significand(final long significand) {
    if (significand == 0) {
      return BerWriter.EMPTY;
    }
    
    return BerWriter.lbyteThen((significand >> 56) & 0xff, significand(significand << 8));
  }
  
  public static BerWriter encode(final double value) {
    if (value == 0) {
      return BerWriter.EMPTY.ibyte(9).ibyte(0);
    }
    
    if (value == Double.POSITIVE_INFINITY) {
      return BerWriter.EMPTY.ibyte(9).ibyte(1).ibyte(0x40);
    }
    
    if (value == Double.NEGATIVE_INFINITY) {
      return BerWriter.EMPTY.ibyte(9).ibyte(1).ibyte(0x41);
    }
    
    final long rawValue = java.lang.Double.doubleToLongBits(value);
    final long sign = (rawValue >> 63) & 0x1;
    final int scale = 0;
    final int base = 0; // binary
    final long rawExponent = (rawValue >> 52) & 0x7ff;
    final long exponent = ((rawValue >> 52) & 0x7ff) - 1023;
    final long mantissa = rawValue & 0x000fffffffffffffL;
    final long encMantissa = (mantissa | 0x00010000000000000L) << 4;
    final BerWriter encodedMantissa = significand(encMantissa);
    final BerWriter encodedExponent = i8sig(BerWriter.EMPTY, exponent);
    final BerWriter encodedDescriptor = BerWriter.EMPTY.lbyte(
        (0x80 | (sign << 6) | (base << 4) | (scale << 2) | ((encodedExponent.length - 1) & 0x3)));
    final BerWriter realData = encodedDescriptor.then(encodedExponent).then(encodedMantissa);
    
    return BerWriter.EMPTY.ibyte(9).ibyte(realData.length).then(realData);
  }
  
  public static BerWriter encode(final AsnReal value) {
    return encode(value.value);
  }
}
