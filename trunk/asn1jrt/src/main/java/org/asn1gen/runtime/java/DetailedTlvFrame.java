package org.asn1gen.runtime.java;

public class DetailedTlvFrame {
  public final ByteArrayWindow window;
  public final TlvFrame frame;
  public final ByteArrayWindow tagWindow;
  public final ByteArrayWindow lengthWindow;
  public final ByteArrayWindow remainder;
  
  public DetailedTlvFrame(
      final ByteArrayWindow window,
      final TlvFrame frame,
      final ByteArrayWindow tagWindow,
      final ByteArrayWindow lengthWindow,
      final ByteArrayWindow remainder) {
    this.window = window;
    this.frame = frame;
    this.tagWindow = tagWindow;
    this.lengthWindow = lengthWindow;
    this.remainder = remainder;
  }
}
