package org.asn1gen.runtime.java;

public interface OnTlv<R> {
  public ByteArrayWindow call(
      final ByteArrayWindow tagWindow,
      final TagClass tagClass,
      final TagForm tagForm,
      final long tagNo,
      final ByteArrayWindow lengthWindow,
      final int tagLength,
      final ByteArrayWindow valueWindow,
      final R[] result);
}
