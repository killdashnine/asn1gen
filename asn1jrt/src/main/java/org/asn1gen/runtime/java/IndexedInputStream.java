package org.asn1gen.runtime.java;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

public class IndexedInputStream extends InputStream {
  private final InputStream is;
  private long index = 0;
  
  public IndexedInputStream(final InputStream is, final long index) {
    this.is = is;
    this.index = index;
  }
  
  public IndexedInputStream(final InputStream is) {
    this(is, 0);
  }
  
  @Override
  public int read() throws IOException {
    final int value = is.read();
    
    if (value == -1) {
      throw new EOFException();
    }
    
    index += 1;
    
    return value;
  }
  
  public long getIndex() {
    return index;
  }
  
  public BoundedInputStream unbounded(final long length) {
    return new BoundedInputStream(this, Long.MAX_VALUE);
  }
  
  public BoundedInputStream until(final long length) {
    return new BoundedInputStream(this, index + length);
  }
}
