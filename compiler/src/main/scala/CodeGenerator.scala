package fumurtCompiler


object FumurtCodeGenerator
{
  def generate(ast:List[Definition]):String =
  {
    val includestatement = "#include <iostream>\n#include <thread>\n#include <string>\n#include <atomic>\n#include <condition_variable>\n#include <list>\n#include <chrono>\n\n\n"
    val topthreads = getthreads(ast)
    val numtopthreads = topthreads.length
    val synchronizationGlobalVars = "static std::atomic<int> rendezvousCounter;\nstatic std::mutex rendezvousSyncMutex;\nstatic std::condition_variable cv;"
    val main = getmain(topthreads)
    
    includestatement + "#define NUMTOPTHREADS " + numtopthreads.toString + "\n" + synchronizationGlobalVars + "\n\n" + main
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
  
  def getmain(topthreads:List[FunctionCallStatement]):String =
  {
    var threadsStart = ""
    
    for(i<-topthreads)
    {
      threadsStart = threadsStart + "\n" + "std::thread t1 (" + i.functionidentifier + ");"
    }
    
    "int main()\n{\nrendezvousCounter.store(0);" + threadsStart + "\n}"
  }
}
