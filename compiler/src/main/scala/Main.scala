package fumurtCompiler

import scala.io.Source._
import scala.util.parsing.input.Positional

object CompileTypeOption extends Enumeration
{
  type CompileTypeOption = Value
  val compiledToGo, compiledToC, compiledToCpp, interpreted = Value 
}

import CompileTypeOption._

object Main
{
  def main(args: Array[String]) :Unit ={
    if(args.length <1)
    {
        println("no file found\n")
    }
    else
    {
      val parts = args(0).split("""\.""")
      if(parts.length==2)
      {
        if(parts(1)=="fumurt")
        {
          compile(getOptions(args.drop(1), args(0)))
        }
        else
        {
          println("unknown file ending\n")
        }
      }
      else
      {
        println("no file found\n")
      }
    }
  }
  
  def getOptions(args:Array[String],file:String): Options =
  {
    println(args.toString)
    new Options(CompileTypeOption.interpreted, true, file)
  }
  
  def compile(opts:Options):Unit =
  {
    println("Now compiling!")
    val sourcestring = fromFile(opts.file).mkString
    FumurtScanner.scan(sourcestring, opts) match
    {
      case Left(error) => println("Error in scanner: " + error.toString)
      case Right(tokens) => 
      {
        println("successful scan. Tokens: "+tokens.toString)
        FumurtParser.parse(tokens) match
        {
          case Left(error) => println("Error in scanner: " + error.toString)
          case Right(ast) => println("Success in parser: " + ast.toString) 
        }
      }
    }
    
  }
  
}

class Options(val compileTypeOption:CompileTypeOption, val debug:Boolean, val file:String)


