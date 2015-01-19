package fumurtCompiler

class Position

case object Global extends Position
case class Source(val line:Int, val column:Int) extends Position

case class FumurtError(val position:Position, val message:String)
