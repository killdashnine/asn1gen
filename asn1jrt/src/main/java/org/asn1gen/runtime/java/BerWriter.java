package org.asn1gen.runtime.java;

import java.io.IOException;
import java.io.DataOutputStream;
import java.nio.charset.Charset;

public abstract class BerWriter {
  public final int length;
  
  protected BerWriter(final int length) {
    this.length = length;
  }
  
  public abstract void write(final DataOutputStream os) throws IOException;

  public static final BerWriter EMPTY = new BerWriter(0) {
    @Override
    public void write(final DataOutputStream os) throws IOException {
    }
  };
  
  public BerWriter bbyte(final byte value) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + 1) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        os.write(((int)value) & 0xff);
      }
    };
  }
  
  public BerWriter bbytes(final byte ...values) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + values.length) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        os.write(values, 0, values.length);
      }
    };
  }

  public BerWriter write(final byte[] values, final int start, final int length) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + length) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        os.write(values, start, length);
      }
    };
  }

  public BerWriter sbytes(final short ...values) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + values.length) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        for (final short value: values) {
          os.write(value & 0xff);
        }
      }
    };
  }

  public BerWriter ibytes(final int ...values) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + values.length) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        for (final int value: values) {
          os.write(value & 0xff);
        }
      }
    };
  }
  
  public BerWriter lbytes(final long ...values) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + values.length) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        for (final long value: values) {
          os.write((int)value & 0xff);
        }
      }
    };
  }

  public BerWriter sbyte(final short value) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + 1) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        os.write(value & 0xff);
      }
    };
  }
  
  public BerWriter ibyte(final int value) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + 1) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        os.write(value & 0xff);
      }
    };
  }
  
  public BerWriter lbyte(final long value) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + 1) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        os.write(((int)value) & 0xff);
      }
    };
  }

  public BerWriter then(final BerWriter berWriter) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + berWriter.length) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        berWriter.write(os);
      }
    };
  }

  public BerWriter after(final BerWriter berWriter) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + berWriter.length) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        berWriter.write(os);
        outer.write(os);
      }
    };
  }
  
  public BerWriter writeVariableInteger(final long value) {
    if (value == 0) {
      return EMPTY.ibyte(0x00);
    } else if (value == -1L) {
      return EMPTY.ibyte(0xff);
    } else {
      return writeVariableInteger(value >> 8).ibyte(0xff);
    }
  }

  public BerWriter i2(final short value) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + 2) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        os.writeShort(value);
      }
    };
  }

  public BerWriter i4(final int value) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + 4) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        os.writeInt(value);
      }
    };
  }
  
  public BerWriter i8(final long value) {
    final BerWriter outer = this;
    return new BerWriter(outer.length + 8) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        os.writeLong(value);
      }
    };
  }
  
  public BerWriter string(final String value, final Charset charset) {
    final BerWriter outer = this;
    final byte[] data = value.getBytes(charset);
    return new BerWriter(outer.length + data.length) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        outer.write(os);
        os.write(data);
      }
    };
  }
}