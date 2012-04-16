package org.asn1gen.runtime.java;

import java.util.Iterator;

public abstract class ConsList<T> implements Iterable<T> {
  private static final ConsList<Object> nil = new ConsList<Object>() {
    public boolean empty() {
      return true;
    }

    @Override
    public Object value() {
      throw new RuntimeException();
    }

    @Override
    public ConsList<Object> tail() {
      throw new RuntimeException();
    }

    @Override
    public Iterator<Object> iterator() {
      return new Iterator<Object>() {
        @Override
        public boolean hasNext() {
          return false;
        }

        @Override
        public Object next() {
          throw new RuntimeException();
        }

        @Override
        public void remove() {
          throw new RuntimeException();
        }
      };
    }
  };
  
  @SuppressWarnings("unchecked")
  public static <T> ConsList<T> nil() {
    return (ConsList<T>)nil;
  }
  
  public abstract boolean empty();
  
  public abstract T value();
  
  public abstract ConsList<T> tail();
  
  public ConsList<T> prepend(final T item) {
    return new Cons<T>(item, this);
  }
  
  public static <T> ConsList<T> cons(final T item, final ConsList<T> tail) {
    return new ConsList<T>() {
      public T value() {
        return item;
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
        final ConsList<T> self = this;
        
        return new Iterator<T>() {
          private ConsList<T> cons = self;
          
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
    };
  }
  
  public ConsList<T> reverse() {
    ConsList<T> target = ConsList.<T>nil();
    
    for (final T item: this) {
      target = target.prepend(item);
    }
    
    return target;
  }
}
