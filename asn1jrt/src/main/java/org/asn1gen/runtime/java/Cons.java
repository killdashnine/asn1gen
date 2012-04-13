package org.asn1gen.runtime.java;

import java.util.Iterator;

public class Cons<T> extends ConsList<T> {
  public final T value;
  public final ConsList<T> tail;
  
  public Cons(final T value, final ConsList<T> tail) {
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
  public ConsList<T> tail() {
    return tail;
  }

  @Override
  public Iterator<T> iterator() {
    return new Iterator<T>() {
      private ConsList<T> cons = Cons.this;
      
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
