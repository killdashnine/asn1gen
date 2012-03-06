package org.asn1gen.runtime.java;

import java.io.IOException;
import java.io.OutputStream;

public interface BerWriter {
  public void write(final OutputStream os) throws IOException;
}
