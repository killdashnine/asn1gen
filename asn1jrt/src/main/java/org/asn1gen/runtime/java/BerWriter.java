package org.asn1gen.runtime.java;

import java.io.IOException;
import java.io.OutputStream;

public abstract class BerWriter {
  public final int length;
  
  protected BerWriter(final int length) {
    this.length = length;
  }
  
  public abstract void write(final OutputStream os) throws IOException;

  public static BerWriter writeByte(final byte value) {
    return new BerWriter(1) {
      @Override
      public void write(final OutputStream os) throws IOException {
        os.write(((int)value) & 0xff);
      }
    };
  }

  public static BerWriter writeUnsignedByte(final int value) {
    return new BerWriter(1) {
      @Override
      public void write(final OutputStream os) throws IOException {
        os.write(value & 0xff);
      }
    };
  }

  public static BerWriter writeUnsignedByte(final long value) {
    return new BerWriter(1) {
      @Override
      public void write(final OutputStream os) throws IOException {
        os.write((int)(value & 0xff));
      }
    };
  }

  public static BerWriter writeBytes(final byte... values) {
    return new BerWriter(values.length) {
      @Override
      public void write(final OutputStream os) throws IOException {
        for (final byte value: values) {
          os.write(((int)value) & 0xff);
        }
      }
    };
  }
  
  public static int bytesIn(final BerWriter... berWriters) {
    int length = 0;
    
    for (final BerWriter berWriter: berWriters) {
      length += berWriter.length;
    }
    
    return length;
  }
  
  public static BerWriter write(final BerWriter... berWriters) {
    return new BerWriter(bytesIn(berWriters)) {
      @Override
      public void write(final OutputStream os) throws IOException {
        for (final BerWriter berWriter: berWriters) {
          berWriter.write(os);
        }
      }
    };
  }

  public static BerWriter writeVariableInteger(final long value) {
    if (value == 0) {
      return writeUnsignedByte(0x0);
    } else if (value == -1L) {
      return writeUnsignedByte(0xff);
    } else {
      return write(
          writeVariableInteger(value >> 8),
          writeUnsignedByte(value & 0xff));
    }
  }
}
