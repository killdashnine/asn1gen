package org.asn1gen.runtime.java;

import java.io.IOException;
import java.io.OutputStream;

public class BerEncoder {
  private static final byte[] TRUE_BYTES = new byte[] { 1, 1, -1 };
  private static final byte[] FALSE_BYTES = new byte[] { 1, 1, 0 };
  private static final byte[] NULL_BYTES = new byte[] { 5, 0, 0 };
  private static final byte[] ZERO_BYTE = new byte[] { 0 };
  
  public BerEncoder() {
  }
  
  public BerWriter encode(final AsnBoolean value) {
    return new BerWriter() {
      public void write(final OutputStream os) throws IOException {
        if (value.value) {
          os.write(TRUE_BYTES);
        } else {
          os.write(FALSE_BYTES);
        }
      }
    };
  }
  
  public BerWriter encode(final AsnNull value) {
    return new BerWriter() {
      public void write(final OutputStream os) throws IOException {
        os.write(NULL_BYTES);
      }
    };
  }
  
  public BerWriter encode(final AsnInteger value) {
    return new BerWriter() {
      public void write(final OutputStream os, final long subValue) throws IOException {
        if (subValue == 0) {
          os.write(0x0);
        } else if (subValue == -1L) {
          os.write(0xff);
        } else {
          write(os, subValue >> 8);
          os.write((int)(subValue & 0xff));
        }
      }
      
      public void write(final OutputStream os) throws IOException {
        write(os, value.value);
      }
    };
  }
}
