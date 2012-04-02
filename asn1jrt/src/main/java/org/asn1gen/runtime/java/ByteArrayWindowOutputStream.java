package org.asn1gen.runtime.java;

import java.io.EOFException;
import java.io.IOException;
import java.io.OutputStream;

public class ByteArrayWindowOutputStream extends OutputStream {
  private final byte[] array;
  public int start;
  public final int length;
  
  public ByteArrayWindowOutputStream(final ByteArrayWindow window) {
    this.array = window.array;
    this.start = window.start;
    this.length = window.length;
  }
  
  public ByteArrayWindow getWindow() {
    return new ByteArrayWindow(array, start, length);
  }

  @Override
  public void write(int byteValue) throws IOException {
    if (start >= length) {
      throw new EOFException();
    }
    
    try {
      this.array[start] = (byte)byteValue;
    } finally {
      start += 1;
    }
  }
}
