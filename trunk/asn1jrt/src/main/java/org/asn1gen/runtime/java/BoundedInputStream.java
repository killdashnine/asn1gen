package org.asn1gen.runtime.java;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

public class BoundedInputStream extends InputStream {
  private final IndexedInputStream is;
  private final long bounds;
  
  public BoundedInputStream(final IndexedInputStream is, final long bounds) {
    this.is = is;
    this.bounds = bounds;
  }
  
  public BoundedInputStream(final IndexedInputStream is) {
    this(is, Long.MAX_VALUE);
  }
  
  @Override
  public int read() throws IOException {
    if (isEmpty()) {
      throw new EOFException();
    }
    
    return is.read();
  }
  
  public boolean isEmpty() {
    return is.getIndex() >= bounds;
  }
}
