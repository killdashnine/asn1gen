package org.asn1gen.runtime.java;

public interface Tail<T> extends Iterable<T> {
  public boolean empty();
  
  public T value();
  
  public Tail<T> tail();
}
