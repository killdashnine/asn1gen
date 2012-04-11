package org.asn1gen.runtime.java;

import java.nio.charset.Charset;

public class BerToAsn {
  public static final BerWriter EMPTY = BerWriter.EMPTY;
  public static final BerWriter ASN_TRUE = BerWriter.EMPTY.ibyte(0x01).ibyte(0x01).ibyte(0xff);
  public static final BerWriter ASN_FALSE = BerWriter.EMPTY.ibyte(0x01).ibyte(0x01).ibyte(0x00);
  public static final BerWriter ASN_NULL = BerWriter.EMPTY.ibyte(0x05).ibyte(0x00);
  public static final BerWriter TRUE = BerWriter.EMPTY.ibyte(0xff);
  public static final BerWriter FALSE = BerWriter.EMPTY.ibyte(0x00);
  public static final BerWriter NULL = BerWriter.EMPTY;
  
  public static BerWriter decode(final AsnBoolean value) {
    return decode(value.value);
  }
  
  public static BerWriter decode(final boolean value) {
    return value ? ASN_TRUE : ASN_FALSE;
  }
  
  public static BerWriter decode(final AsnNull value) {
    return ASN_NULL;
  }

  public static BerWriter decode(final long value) {
    final BerWriter dataWriter = decodePart(value);
    // TODO: Use proper length
    return BerWriter.EMPTY.ibyte(2).ibyte(dataWriter.length).then(dataWriter);
  }
  
  public static BerWriter decode(final AsnInteger value) {
    final BerWriter dataWriter = decodePart(value);
    // TODO: Use proper length
    return BerWriter.EMPTY.ibyte(2).ibyte(dataWriter.length).then(dataWriter);
  }
  
  public static BerWriter decode(final double value) {
    final BerWriter dataWriter = decodePart(value);
    // TODO: Use proper length
    return BerWriter.EMPTY.ibyte(9).ibyte(dataWriter.length).then(dataWriter);
  }
  
  public static BerWriter decode(final AsnReal value) {
    final BerWriter dataWriter = decodePart(value);
    // TODO: Use proper length
    return BerWriter.EMPTY.ibyte(9).ibyte(dataWriter.length).then(dataWriter);
  }
  
  public static BerWriter decode(final AsnBitString value) {
    final BerWriter dataWriter = decodePart(value);
    // TODO: Use proper length
    return EMPTY.ibyte(3).ibyte(dataWriter.length).then(dataWriter);
  }

  public static BerWriter decode(final AsnOctetString value) {
    final BerWriter dataWriter = decodePart(value);
    // TODO: Use proper length
    return EMPTY.ibyte(4).ibyte(dataWriter.length).then(dataWriter);
  }

  public static BerWriter decode(final AsnUtf8String value) {
    final BerWriter dataWriter = decodePart(value);
    // TODO: Use proper length
    return EMPTY.ibyte(12).ibyte(dataWriter.length).then(dataWriter);
  }

  public static BerWriter decodePart(final AsnBoolean value) {
    return decodePart(value.value);
  }
  
  public static BerWriter decodePart(final boolean value) {
    return value ? TRUE : FALSE;
  }
  
  public static BerWriter decodePart(final AsnNull value) {
    return NULL;
  }

  public static BerWriter i8sig(final long value) {
    final long excessValue = value >> 8;
    final long capturedValue = value & 0xff;
    
    if (excessValue == -1) {
      return EMPTY.lbyte(capturedValue);
    } else if (excessValue > 0) {
      return i8sig(excessValue).lbyte(capturedValue);
    } else {
      return EMPTY.lbyte(capturedValue);
    }
  }
  
  public static BerWriter decodePart(final long value) {
    return BerWriter.EMPTY.writeVariableInteger(value);
  }
  
  public static BerWriter decodePart(final AsnInteger value) {
    return decodePart(value.value);
  }
  
  public static int trailingZeros(final long value, final int shiftTest) {
    if (shiftTest == 1) {
      final long nibble = value & 3;
      
      if (nibble == 0) {
        return 2;
      }
      
      if (nibble == 2) {
        return 1;
      }
      
      return 0;
    }
    
    final long bitMask = (1L << shiftTest) - 1;
    
    if ((value & bitMask) == 0) {
      return shiftTest + trailingZeros(value >> shiftTest, shiftTest / 2);
    } else {
      return trailingZeros(value, shiftTest / 2);
    }
  }
  
  public static int trailingZeros(final long value) {
    return trailingZeros(value, 32);
  }
  
  public static BerWriter decodePart(final double value) {
    if (value == 0) {
      return BerWriter.EMPTY;
    }
    
    if (value == Double.POSITIVE_INFINITY) {
      return BerWriter.EMPTY.ibyte(0x40);
    }
    
    if (value == Double.NEGATIVE_INFINITY) {
      return BerWriter.EMPTY.ibyte(0x41);
    }
    
    final long rawValue = java.lang.Double.doubleToLongBits(value);
    final long sign = (rawValue >> 63) & 0x1;
    final int scale = 0;
    final int base = 0; // binary
    final long exponent = ((rawValue >> 52) & 0x7ff) - 1023;
    final long mantissa = (rawValue & 0x000fffffffffffffL) | 0x0010000000000000L;
    final long mantissaShift = trailingZeros(mantissa);
    final long significand = mantissa >> mantissaShift;
    final long trueExponent = exponent - 52 + mantissaShift;
    final BerWriter decodedMantissa = i8sig(significand);
    final BerWriter decodedExponent = i8sig(trueExponent);
    final BerWriter decodedDescriptor = BerWriter.EMPTY.lbyte(
        (0x80 | (sign << 6) | (base << 4) | (scale << 2) | ((decodedExponent.length - 1) & 0x3)));
    return decodedDescriptor.then(decodedExponent).then(decodedMantissa);
  }
  
  public static BerWriter decodePart(final AsnReal value) {
    return decodePart(value.value);
  }
  
  private static BerWriter decodeBitStringBits(final long value, final int length) {
    if (length <= 0) {
      return EMPTY;
    }
    
    return decodeBitStringBits(value >> 8, length - 1).lbyte(value & 0xff);
  }

  public static BerWriter decodePart(final AsnBitString value) {
    final int excess = (64 - value.length) % 8;
    final int decodeLength = (value.length + excess) / 8;
    return EMPTY.ibyte(excess).then(decodeBitStringBits(value.value << excess, decodeLength));
  }

  public static BerWriter decodePart(final AsnOctetString value) {
    return EMPTY.bbytes(value.value);
  }

  public static BerWriter decodePart(final AsnUtf8String value) {
    return EMPTY.bbytes(value.value.getBytes(Charset.forName("UTF-8")));
  }
}
