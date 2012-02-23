package org.asn1gen.gen

import _root_.java.io.File
import _root_.org.asn1gen.extra.Extras._
import _root_.org.asn1gen.io._

object GenTuples {
  def genN(out: IndentWriter, prefix: String, count: Int) = {
    for (i <- 1 to count) {
      if (i != 1) {
        out.print(", ")
      }
      out.print(prefix)
      out.print(i)
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val start: Int = _root_.java.lang.Integer.parseInt(args(0))
    val end: Int = _root_.java.lang.Integer.parseInt(args(1))
    val outdir = new File("out/scala")
    outdir.mkdirs
    
    for (i <- start to end) {
      val functionFile = outdir("Function" + i + ".scala")
      functionFile.withIndentWriter { out =>
        out.println("package scala")
        out.println
        out.println("/**")
        out.println(" * <p>")
        out.println(" * Function with " + i + " parameters.")
        out.println(" * </p>")
        out.println(" */")
        out.print("trait Function" + i + "[")
        for (j <- 1 to i) {
          out.print("-T" + j + ", ")
        }
        out.println("+R] extends AnyRef { self =>")
        out.indent {
          out.print("def apply(")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("v" + j + ": T" + j)
          }
          out.println("): R")
          out.println
          out.println("override def toString() = \"<function" + i + ">\"")
          out.println
          out.println("/**")
          out.print(" * f(")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("x" + j)
          }
          out.print(") == (f.tupled)(Tuple" + i + "(")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("x" + j)
          }
          out.println("))")
          out.println(" */")
          out.print("def tupled: Tuple" + i + "[")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("T" + j)
          }
          out.println("] => R = {")
          out.indent {
            out.print("case Tuple" + i + "(")
            for (j <- 1 to i) {
              if (j != 1) {
                out.print(", ")
              }
              out.print("x" + j)
            }
            out.print(") => apply(")
            for (j <- 1 to i) {
              if (j != 1) {
                out.print(", ")
              }
              out.print("x" + j)
            }
            out.println(")")
          }
          out.println("}")
        }
        out.println("}")
      }
    
      val productFile = outdir("Product" + i + ".scala")
      productFile.withIndentWriter { out =>
        out.println("package scala")
        out.println
        out.println("object Product" + i + " {")
        out.indent {
          out.print("def unapply[")
          genN(out, "T", i)
          out.print("](x: Product" + i + "[")
          genN(out, "T", i)
          out.print("]): Option[Product" + i + "[")
          genN(out, "T", i)
          out.println("]] =")
          out.indent {
            out.println("Some(x)")
          }
        }
        out.println("}")
        out.println
        out.println("/**")
        out.println(" * Product" + i + " is a cartesian product of " + i + " components.")
        out.println(" * ")
        out.println(" * @since 999.999")
        out.println(" */")
        out.print("trait Product" + i + "[")
        genN(out, "+T", i)
        out.println("] extends Product {")
        out.indent {
          out.println("/**")
          out.println(" * The arity of this product.")
          out.println(" * @return " + i)
          out.println(" */")
          out.println("override def productArity = " + i)
          out.println
          out.println("/**")
          out.println(" * Returns the n-th projection of this product if 0&lt;=n&lt;arity,")
          out.println(" * otherwise null.")
          out.println(" *")
          out.println(" * @param n number of the projection to be returned ")
          out.println(" * @return same as _(n+1)")
          out.println(" * @throws IndexOutOfBoundsException")
          out.println(" */")
          out.println("@throws(classOf[IndexOutOfBoundsException])")
          out.println("override def productElement(n: Int) = n match { ")
          out.indent {
            for (j <- 1 to i) {
              out.println("case " + (j - 1) + " => _" + j)
            }
            out.println("case _ => throw new IndexOutOfBoundsException(n.toString())")
          }
          out.println("}")
          for (j <- 1 to i) {
            out.println
            out.println("/** projection of this product */")
            out.println("def _" + j + ": T" + j)
          }
        }
        out.println("}")
      }
      val tupleFile = outdir("Tuple" + i + ".scala")
      tupleFile.withIndentWriter { out =>
        out.println("package scala")
        out.println
        out.println("import scala.collection.{TraversableLike, IterableLike}")
        out.println("import scala.collection.generic.CanBuildFrom")
        out.println
        out.println("/**")
        out.println(" * Tuple" + i + " is the canonical representation of a @see Product" + i)
        out.println(" */")
        out.print("case class Tuple" + i + "[")
        genN(out, "+T", i)
        out.print("](")
        for (j <- 1 to i) {
          if (j != 1) {
            out.print(", ")
          }
          out.print("_" + j + ": T" + j)
        }
        out.println(")")
        out.indent {
          out.print("extends Product" + i + "[")
          genN(out, "T", i)
          out.println("]")
        }
        out.println("{")
        out.indent {
          out.print("override def toString() = \"(")
          for (j <- 1 to i) {
            if (j != 1) {
              out.print(", ")
            }
            out.print("\" + _" + j + " + \"")
          }
          out.println(")\"")
        }
        out.println("}")
      }
    }
  }
}
