package org.asn1gen.gen.scala

import java.io.File
import org.asn1gen.extra.Extras._
import org.asn1gen.io.IndentWriter
import org.asn1gen.parsing.asn1.Asn1Parser
import org.asn1gen.parsing.asn1.{ast => ast}
import scala.collection.immutable._
import scala.io.Source

case class ScalaModel(modules: HashMap[String, Module]) extends Asn1Parser {
  def parse[N](root: Parser[N], input: String) =
    phrase(root)(new lexical.Scanner(input))
  
  def generateTo(packageName: String, directory: File): Unit = {
    modules foreach { case (name, module) =>
      new File(directory, name + ".scala").openPrintStream { ps =>
        val genScala = new GenScala(packageName, new IndentWriter(ps))
        genScala.generate(module)
      }
    }
  }
  
  def load(file: File): ScalaModel = {
    val text = Source.fromFile(file).mkString
    parse(root, text) match {
      case Success(moduleDefinition, _) =>
        val refactoredModuleDefinition = AnonymousTypeNamer.process(moduleDefinition)
        //GenScalaAst.generate(new IndentWriter(System.out), refactoredModuleDefinition)
        val name = refactoredModuleDefinition.name
        if (modules.contains(name)) {
          throw new ModuleLoadException("Module " + name + " already exists")
        }
        ScalaModel(modules + (refactoredModuleDefinition.name -> Module.from(refactoredModuleDefinition)))
      case failure =>
        throw new ModuleLoadException("Parse failure: " + failure)
    }
  }
  
  def genScala(file: File): Unit = {
    file.mkdirs
    modules foreach { module =>
    }
  }
  
  def writeTo(outDirectory: File): Unit = {
    outDirectory.mkdir
    val metaDirectory = outDirectory.child("meta")
    metaDirectory.mkdir
    val codecDirectory = outDirectory.child("codec")
    codecDirectory.mkdir
    val berDirectory = codecDirectory.child("ber")
    berDirectory.mkdir
    modules foreach { case (moduleName, module) =>
      val moduleFile = outDirectory.child(moduleName + ".scala")
      moduleFile.openPrintStream { ps =>
        val genScala = new GenScala("moo", new IndentWriter(ps))
        genScala.generate(module)
        println("Writing to " + moduleFile)
      }
    }
    modules foreach { case (moduleName, module) =>
      val moduleFile = metaDirectory.child(moduleName + ".scala")
      moduleFile.openPrintStream { ps =>
        val genScala = new GenScalaMeta("moo", new IndentWriter(ps))
        genScala.generate(module)
        println("Writing to " + moduleFile)
      }
    }
    modules foreach { case (moduleName, module) =>
      val moduleFile = berDirectory.child(moduleName + ".scala")
      moduleFile.openPrintStream { ps =>
        val genScala = new GenScalaBerEncoder("moo", new IndentWriter(ps))
        genScala.generate(module)
        println("Writing to " + moduleFile)
      }
    }
  }
}

object ScalaModel {
  def empty = ScalaModel(HashMap[String, Module]())
}
