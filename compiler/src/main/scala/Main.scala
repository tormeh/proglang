package fumurtCompiler

import scala.io.Source._
import scala.util.parsing.input.Positional

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
          compile(getOptions(parts(0), args.drop(1)))
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
  
  def getOptions(args:Array[String],file:String): =
  {
  
  }
  
}
