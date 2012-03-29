package org.asn1gen.runtime.java;

import java.io.PrintStream;
import java.io.PrintWriter;

public class TLV {
  public static void dump(final PrintStream out, final ByteArrayWindow window) {
    try (final IndentWriter indentWriter = new IndentWriter(out)) {
      dump(indentWriter, window);
    }
  }

  public static void dump(final PrintWriter out, final ByteArrayWindow window) {
    try (final IndentWriter indentWriter = new IndentWriter(out)) {
      dump(indentWriter, window);
    }
  }

  public static void dump(final IndentWriter out, final ByteArrayWindow window) {
    if (window.length == 0) {
      return;
    }
    
    final byte tagByte = window.get(0);
    
    if (window.length == 1) {
      out.$("<missing tag part>");
      return;
    }
    
    final ByteArrayWindow afterTagWindow = window.from(1);
    final int shortTag = tagByte & 0x1f;
    final Tuple2<Integer, ByteArrayWindow> tagTuple = tagOf(shortTag, afterTagWindow);
    final ByteArrayWindow lengthWindow = afterTagWindow.until(afterTagWindow.length - tagTuple.b.length);
    final int tag = tagTuple.a;
    
    for (int i = 0; i < lengthWindow.length; ++i) {
      out.hex(lengthWindow.get(i)).$(' ');
    }
    
    out.hex(tagByte).$(' ').$('[');
    
    final Delimeter delimeter = new Delimeter("", " ");
    
    out.$(delimeter).$(TagClass.fromTagByte(tagByte));
    
    if ((tagByte & 0x20) != 0) {
      out.$(delimeter).$("CONSTRUCTED");
    } else {
      out.$(delimeter).$("PRIMITIVE");
    }
    
    out.$(delimeter).$(tag).$("] {").endln();
    
    try (final Indent indent = out.indent(2)) {
      out.$("Hello").endln();
    }
    
    out.$("}");
  }
  
  public static Tuple2<Integer, ByteArrayWindow> tagOf(
      final int shortLength,
      final ByteArrayWindow window) {
    if (shortLength < 32) {
      return new Tuple2<Integer, ByteArrayWindow>(shortLength, window);
    }
    
    return tagOf(window, 0);
  }
    
  public static Tuple2<Integer, ByteArrayWindow> tagOf(
      final ByteArrayWindow window,
      final int accumulator) {
    if (window.length == 0) {
      return null;
    }
    
    final int value = (accumulator << 7) + (window.get(0) & (int)0x7f);
    
    if ((window.get(0) & 0x80) != 0) {
      return tagOf(window.from(1), value);
    }
    
    return new Tuple2<Integer, ByteArrayWindow>(value, window.from(1));
  }
  
  public static void main(final String[] args) {
    dump(System.out, ByteArrayWindow.to(new byte[] { 0x31, 0x0c, 0x04, 0x00, 0x04, 0x00, 0x04, 0x00, 0x0a, 0x01, 0x00, 0x01, 0x01, 0x00 }));
  }
}
