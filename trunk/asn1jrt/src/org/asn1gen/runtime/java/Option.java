package org.asn1gen.runtime.java;

public interface Option<T> extends Iterable<T> {
  public T value();

  public boolean empty();
}
