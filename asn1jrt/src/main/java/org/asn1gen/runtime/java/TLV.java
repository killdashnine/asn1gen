package org.asn1gen.runtime.java;

import java.io.PrintStream;
import java.io.PrintWriter;

public class TLV {
  public static void dump(final PrintStream out, final byte[] buffer) {
    try (final IndentWriter indentWriter = new IndentWriter(out)) {
      dump(indentWriter, buffer);
    }
  }

  public static void dump(final PrintWriter out, final byte[] buffer) {
    try (final IndentWriter indentWriter = new IndentWriter(out)) {
      dump(indentWriter, buffer);
    }
  }

  public static void dump(final IndentWriter out, final byte[] buffer) {
    out.println("Hello");
    try (final Indent indent = out.indent(2)) {
      out.println("Hello");
    }
  }
  
  public static void main(final String[] args) {
    dump(System.out, new byte[] { 0x31, 0x0c, 0x04, 0x00, 0x04, 0x00, 0x04, 0x00, 0x0a, 0x01, 0x00, 0x01, 0x01, 0x00 });
  }
}
