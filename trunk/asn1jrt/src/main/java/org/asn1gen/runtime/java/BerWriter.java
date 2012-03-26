package org.asn1gen.runtime.java;

import java.io.ByteArrayOutputStream;
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
    if ((value & 0xffffffffffffff00L) == 0xffffffffffffff00L) {
      return EMPTY.lbyte(value);
    } else if ((value | 0x00000000000000ffL) == 0x00000000000000ffL) {
      return EMPTY.lbyte(value);
    } else {
      return writeVariableInteger(value >> 8).lbyte(value & 0xff);
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

  public static BerWriter lbyteThen(final long value, final BerWriter that) {
     return new BerWriter(that.length + 1) {
      @Override
      public void write(final DataOutputStream os) throws IOException {
        os.write(((int)value) & 0xff);
        that.write(os);
      }
    };
  }
  
  public void dumpln() {
    dump();
    System.out.println();
  }
  
  public void dump() {
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    final DataOutputStream dos = new DataOutputStream(baos);
    try {
      try {
        try {
          this.write(dos);
        } finally {
          dos.flush();
        }
      } finally {
        baos.flush();
      }
    } catch (final IOException e) {
      e.printStackTrace(System.err);
    }
    
    boolean first = true;
    final String hex = "0123456789abcdef";
    for (final byte b: baos.toByteArray()) {
      if (!first) {
        System.out.print(" ");
      }
      System.out.print(hex.charAt((b >> 4) & 0xf));
      System.out.print(hex.charAt(b & 0xf));
    }
  }
  
  public BerWriter length(final long value) {
    if (value < 0) {
      throw new IllegalArgumentException();
    }
    
    if (value <= 127) {
      return this.lbyte(value);
    }
    
    return this.lengthInit(value >> 7).lbyte(value & 0x7f);
  }

  private BerWriter lengthInit(final long value) {
    if (value < 0) {
      throw new IllegalArgumentException();
    }
    
    if (value <= 127) {
      return this.lbyte((value & 0x7f) | 0x80);
    }
    
    return this.lengthInit(value >> 7).lbyte((value & 0x7f) | 0x80);
  }
}
