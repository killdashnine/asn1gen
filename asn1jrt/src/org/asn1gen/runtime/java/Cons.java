package org.asn1gen.runtime.java;

import java.util.Iterator;

public class Cons<T> implements Tail<T> {
  public final T value;
  public final Tail<T> tail;
  
  public Cons(final T value, final Tail<T> tail) {
    this.value = value;
    this.tail = tail;
  }
  
  public T value() {
    return value;
  }
  
  public boolean empty() {
    return false;
  }

  @Override
  public Tail<T> tail() {
    return tail;
  }

  @Override
  public Iterator<T> iterator() {
    return new Iterator<T>() {
      private Tail<T> cons = Cons.this;
      
      @Override
      public boolean hasNext() {
        return !cons.empty();
      }

      @Override
      public T next() {
        final T value = cons.value();
        cons = cons.tail();
        return value;
      }

      @Override
      public void remove() {
        throw new RuntimeException();
      }
    };
  }
}
