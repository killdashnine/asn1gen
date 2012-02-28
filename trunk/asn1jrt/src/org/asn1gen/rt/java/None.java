package org.asn1gen.rt.java;

import java.util.Iterator;

public class None<T> implements Option<T> {
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
