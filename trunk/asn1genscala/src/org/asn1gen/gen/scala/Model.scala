package org.asn1gen.gen.scala

import org.asn1gen.io._
import org.asn1gen.io.JavaTypes._
import org.asn1gen.io.RichFile.fromJava
import org.asn1gen.io.RichFile.toJava
import org.asn1gen.parsing.asn1.Asn1Parser
import org.asn1gen.parsing.asn1.{ast => ast}
import scala.collection.immutable._
import scala.io.Source

case class Model (modules: HashMap[String, Module]) extends Asn1Parser {
  def parse[N](root: Parser[N], input: String) =
    phrase(root)(new lexical.Scanner(input))

  def load(file: JavaFile): Model = {
    file.openPrintStream { ps =>
      val text = Source.fromFile(file).mkString
      parse(root, text) match {
        case Success(moduleDefinition, _) =>
          val genScala = new GenScala(new IndentWriter(ps))
          genScala.moduleName = Some("test.asn1.genruntime")
          genScala.generate(moduleDefinition)
          val name = moduleDefinition.name
          if (modules.contains(name)) {
            throw new ModuleLoadException("Module " + name + " already exists")
          }
          Model(modules + (name -> Module.from(moduleDefinition)))
        case failure =>
          throw new ModuleLoadException("Parse failure: " + failure)
      }
    }
  }
  
  def genScala(file: JavaFile): Unit = {
    file.mkdirs
    modules foreach { module =>
    }
  }
}
