package org.asn1gen.runtime.java;

import java.util.Iterator;

public class None<T> implements Option<T> {
  private static final None<Object> instance_ = new None<Object>();
  
  @SuppressWarnings("unchecked")
  public static <T> None<T> instance() {
    return (None<T>)instance_;
  }
  
  public boolean empty() {
    return true;
  }

  @Override
  public T value() {
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
