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
  
  def generateTo(packageName: String, directory: JavaFile): Unit = {
    modules foreach { case (name, module) =>
      new JavaFile(directory, name + ".scala").openPrintStream { ps =>
        val genScala = new GenScala(packageName, new IndentWriter(ps))
        genScala.generate(module)
      }
    }
  }
  
  def load(file: JavaFile): Model = {
    val text = Source.fromFile(file).mkString
    parse(root, text) match {
      case Success(moduleDefinition, _) =>
        val refactoredModuleDefinition = AnonymousTypeNamer.process(moduleDefinition)
        val name = refactoredModuleDefinition.name
        if (modules.contains(name)) {
          throw new ModuleLoadException("Module " + name + " already exists")
        }
        Model(modules + (refactoredModuleDefinition.name -> Module.from(refactoredModuleDefinition)))
      case failure =>
        throw new ModuleLoadException("Parse failure: " + failure)
    }
  }
  
  def genScala(file: JavaFile): Unit = {
    file.mkdirs
    modules foreach { module =>
    }
  }
  
  def writeTo(outDirectory: JavaFile): Unit = {
    outDirectory.mkdir
    modules foreach { case (moduleName, module) =>
      val moduleFile = outDirectory.child(moduleName + ".scala")
      moduleFile.openPrintStream { ps =>
        val genScala = new GenScala("moo", new IndentWriter(ps))
        genScala.generate(module)
        println("Writing to " + moduleFile)
      }
    }
  }
}

object Model {
  def empty = Model(HashMap[String, Module]())
}
