package org.asn1gen.runtime.java;

public interface ConsList<T> extends Iterable<T> {
  public boolean empty();
  
  public T value();
  
  public ConsList<T> tail();
}
