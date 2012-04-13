package org.asn1gen.runtime.java;

import java.util.Iterator;

public class Nil<T> extends ConsList<T> {
  private static final Nil<Object> instance_ = new Nil<Object>();
  
  @SuppressWarnings("unchecked")
  public static <T> Nil<T> instance() {
    return (Nil<T>)instance_;
  }
  
  public boolean empty() {
    return true;
  }

  @Override
  public T value() {
    throw new RuntimeException();
  }

  @Override
  public ConsList<T> tail() {
    throw new RuntimeException();
  }

  @Override
  public Iterator<T> iterator() {
    return new Iterator<T>() {
      @Override
      public boolean hasNext() {
        return false;
      }

      @Override
      public T next() {
        throw new RuntimeException();
      }

      @Override
      public void remove() {
        throw new RuntimeException();
      }
    };
  }
}
