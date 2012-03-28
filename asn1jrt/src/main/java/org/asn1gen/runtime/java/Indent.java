package org.asn1gen.runtime.java;

public class Indent implements AutoCloseable {
  private final IndentWriter writer;
  private final int offset;
  
  public Indent(final IndentWriter writer, final int offset) {
    this.offset = offset;
    this.writer = writer;
    this.writer.indentByOffset(offset);
  }
  
  @Override
  public void close() {
    this.writer.indentByOffset(-offset);
  }
}
