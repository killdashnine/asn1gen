package org.asn1gen.runtime.java;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Writer;

public class IndentWriter extends PrintWriter {
  private int indent = 0;
  private int emptyLines = -1;
  private int line = 0;
  
  public IndentWriter(final Writer out) {
    super(out, true);
  }
  
  public IndentWriter(final PrintStream out) {
    super(new PrintWriter(out, true));
  }
  
  public boolean atLineStart() {
    return emptyLines != -1;
  }
  
  public IndentWriter ensureEmptyLine() {
    return ensureEmptyLines(1);
  }
  
  public IndentWriter ensureEmptyLines(final int lines) {
    while (emptyLines < lines) {
      this.println();
    }
    
    return this;
  }
  
  public void indentByOffset(final int offset) {
    indent += offset;
  }
  
  public Indent indent(final int offset) {
    return new Indent(this, offset);
  }
  
  @Override
  public void println() {
    super.println();
    emptyLines += 1;
    line += 1;
  }

  private void prePrint() {
    if (atLineStart()) {
      for (int i = 0; i < indent; ++i) {
        super.print(" ");
      }
      
      emptyLines = -1;
    }
  }
  
  public void print(final String s) {
    if (s.length() != 0) {
      prePrint();
    }
    super.print(s);
  }
  
  public void print(final char value) {
    prePrint();
    super.print(value);
  }
  
  public void print(final int value) {
    prePrint();
    super.print(value);
  }
  
  public void print(final long value) {
    prePrint();
    super.print(value);
  }
  
  public void print(final float value) {
    prePrint();
    super.print(value);
  }
  
  public void print(final double value) {
    prePrint();
    super.print(value);
  }

  public IndentWriter $(final String s) {
    if (s.length() != 0) {
      prePrint();
    }
    
    super.print(s);
    
    return this;
  }
  
  public IndentWriter $(final char value) {
    prePrint();
    super.print(value);
    
    return this;
  }
  
  public IndentWriter $(final int value) {
    prePrint();
    super.print(value);
    
    return this;
  }
  
  public IndentWriter $(final long value) {
    prePrint();
    super.print(value);
    
    return this;
  }
  
  public IndentWriter $(final float value) {
    prePrint();
    super.print(value);
    
    return this;
  }
  
  public IndentWriter $(final double value) {
    this.print(value);
    
    return this;
  }
  
  public IndentWriter $(final boolean value) {
    this.print(value);
    
    return this;
  }
  
  public IndentWriter $(final Object value) {
    this.print(value.toString());
    
    return this;
  }
  
  public IndentWriter endln() {
    this.println();
    
    return this;
  }
  
  public IndentWriter break$() {
    if (!atLineStart()) {
      this.println();
    }
    
    return this;
  }
  
  private IndentWriter nibble(final int nibble) {
    return $(hexString.charAt(nibble));
  }
  
  public IndentWriter hex(final byte value) {
    return nibble((value >> 4) & 0xf).nibble(value & 0xf);
  }

  private static String hexString = "0123456789abcdef";
  
  public IndentWriter trace(final String left, final String right) {
    try {
      throw new Exception();
    } catch (final Exception e) {
      final StackTraceElement frame = e.getStackTrace()[1];
      this.$(left).$(frame.getFileName()).$(":").$(frame.getLineNumber()).$(right);
    }
    
    return this;
  }
  
  public int getLine() {
    return line;
  }
}
