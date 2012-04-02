package org.asn1gen.runtime.java;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

public class ByteArrayWindowInputStream extends InputStream {
  private final byte[] array;
  public int start;
  public final int length;
  
  public ByteArrayWindowInputStream(final ByteArrayWindow window) {
    this.array = window.array;
    this.start = window.start;
    this.length = window.length;
  }
  
  @Override
  public int read() throws IOException {
    if (start >= length) {
      throw new EOFException();
    }
    
    try {
      return this.array[start];
    } finally {
      start += 1;
    }
  }
  
  public ByteArrayWindow getWindow() {
    return new ByteArrayWindow(array, start, length);
  }
}
