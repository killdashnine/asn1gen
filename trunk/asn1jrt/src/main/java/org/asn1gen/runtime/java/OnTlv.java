package org.asn1gen.runtime.java;

public interface OnTlv<R> {
  public ByteArrayWindow call(
      final TlvFrame frame,
      final ByteArrayWindow tagWindow,
      final ByteArrayWindow lengthWindow,
      final R[] result);
}
