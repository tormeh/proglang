package fumurtCompiler


object FumurtCodeGenerator
{
  def generate(ast:List[Definition]):String =
  {
    val includestatement = "#include <iostream>\n#include <thread>\n#include <string>\n#include <atomic>\n#include <condition_variable>\n#include <list>\n#include <chrono>\n\n\n"
    val topthreads = getthreads(ast)
    val numtopthreads = topthreads.length
    
    includestatement + "#define NUMTOPTHREADS " + numtopthreads.toString
  }
  
  def getthreads(ast:List[Definition]):List[FunctionCallStatement]=
  {
    ast.find(x => (x.leftside.description match {case ProgramT() => true; case _=> false})) match
    {
      case None => println("Error in getthreads. Should be caught by checker."); scala.sys.exit()
      case Some(res) => 
      {
        res.rightside.expressions.flatMap(x => x match
        {
          case x:FunctionCallStatement => if (x.functionidentifier.startsWith("thread")) {Some(x)} else {None}
          case _ => None
        })
      }
    }
  }
}
