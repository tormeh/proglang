package fumurtCompiler

import scala.util.parsing.input.Position
//import scala.util.parsing.input.NoPosition

case object Global extends Position
{
  def column:Int = 0
  def line:Int = 0
  protected def lineContents:String = "global position"
}
case class Source(val line:Int, val column:Int, val lineContents:String) extends Position

case class FumurtError(val position:Position, val message:String)
{
  override def toString:String=
  {
    position.toString + ": " + message + "\n" + position.longString + "\n"
  }
}
