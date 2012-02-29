package org.asn1gen.runtime.java;

public interface List<T> extends Iterable<T> {
  public boolean empty();
  
  public T value();
  
  public List<T> tail();
}
