package fumurtCompiler


object FumurtCodeGenerator
{
  def generate(ast:List[Definition]):String =
  {
    val includestatement = "#include <iostream>\n#include <thread>\n#include <string>\n#include <atomic>\n#include <condition_variable>\n#include <list>\n#include <chrono>\n\n\n"
    val topthreads = gettopthreadstatements(ast)
    val numtopthreads = topthreads.length
    val synchronizationGlobalVars = "static std::atomic<int> rendezvousCounter;\nstatic std::mutex rendezvousSyncMutex;\nstatic std::condition_variable cv;"
    val main = getmain(topthreads)
    val synchvars = getsynchronizedvariables(ast)
    val syncfunc = getsynchonizerfunction(synchvars)
    val synchvardeclarations = getGlobalSynchVariableDeclarations(synchvars)
    val printdecs = getprintlistdeclarations(topthreads)
    val topthreaddeclarations = gettopthreaddeclarations(ast)
    
    includestatement + "#define NUMTOPTHREADS " + numtopthreads.toString + "\n" + synchvardeclarations + printdecs + "\n" + synchronizationGlobalVars + syncfunc + "\n\n" + topthreaddeclarations + "\n" + main
  }
  
  def gettopthreaddeclarations(ast:List[Definition]):String =
  {
    val strings = ast.filter(x => (x.leftside.description match {case ThreadT() => true; case _=> false})).map(x=>
    {   
        val (tailrecursestart, tailrecurseend) = x.rightside.expressions.last match
        {
          case FunctionCallStatement(functionidentifier, args) =>
          {
            if(functionidentifier == x.leftside.id.value)
            {
              //tail recursion! Woo!
              ("While(true)\n{\n","\n}")
            }
            else {println("Error in gettopthreaddeclarations. Not implemented."); scala.sys.exit()}
          }
          case _=> ("","")
        }
        x.rightside.expressions.map(
        x=> x match
        {
          case 
        }
        )
    })
    
    var string=""
    for(i<-strings)
    {
      string+=i
    }
    string
  }
  
  def gettopthreadstatements(ast:List[Definition]):List[FunctionCallStatement]=
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
  
  def getprintlistdeclarations(topthreads:List[FunctionCallStatement]):String=
  {
    val topthreadnames = topthreads.map(x=>x.functionidentifier)
    var out = ""
    for(i<-topthreadnames)
    {
      out += "\nstatic std::list<std::string> print"+i+";"
    }
    out
  }
  
  def getmain(topthreads:List[FunctionCallStatement]):String =
  {
    var threadsStart = ""
    
    for(i<-topthreads)
    {
      threadsStart = threadsStart + "\n" + "std::thread t" + i.functionidentifier + " (" + i.functionidentifier + ");"
    }
    
    "int main()\n{\nrendezvousCounter.store(0);" + threadsStart + "\nwhile(true)\n{\nstd::this_thread::sleep_for(std::chrono::seconds(1));\n}" + "\n}"
  }
  
  def getsynchonizerfunction(synchvariables:List[Definition]):String=
  {
    var synchvariablestrings = ""
    
    for(i<-synchvariables)
    {
      val name = i.leftside.id.value
      synchvariablestrings += name + " = write" + name.capitalize + ";\n"
      
    }
    
    """static void waitForRendezvous(std::string name)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  ++rendezvousCounter;
  if(rendezvousCounter.load() < NUMTOPTHREADS)
  {
    cv.wait(lk);
  }
  else if (rendezvousCounter.load() == NUMTOPTHREADS)
  {
    while(!print1.empty())
    {
      std::cout << print1.front();
      print1.pop_front();
    }
    while(!print2.empty())
    {
      std::cout << print2.front();
      print2.pop_front();
    }
    """ + synchvariablestrings + """
    {
      rendezvousCounter.store(0);
      cv.notify_all();
    }
  }
  else
  {
    std::cout << "error in wait for " << name << ". Rendezvouscounter out of bounds. RedezvousCounter = " << rendezvousCounter.load() << "\n";
    exit(0);
  }
}"""
  }
  
  def getGlobalSynchVariableDeclarations(synchvariables:List[Definition]):String=
  {
    var synchdeclares = ""
    for(i<-synchvariables)
    {
      val fumurttype = i.leftside.returntype.value
      val initialValue = i.rightside.expressions(0) match
      {
        case FunctionCallStatement(functionidentifier, args) => args match
        {
          case Right(namedcallargs) => namedcallargs.value(0).argument match
          {
            case IntegerStatement(value) => value
            case _=> println("Error in getGlobalSynchVariableDeclarations. Should be caught by checker."); scala.sys.exit()
          }
          case _=> println("Error in getGlobalSynchVariableDeclarations. Should be caught by checker."); scala.sys.exit()
        }
        case _=> println(i.rightside.expressions(0).toString); println("Error in getGlobalSynchVariableDeclarations. Should be caught by checker."); scala.sys.exit()
      }
      if (fumurttype == "Integer") 
      {
        synchdeclares += "\nstatic int " + i.leftside.id.value + " = " + initialValue
        synchdeclares += "\nstatic int write" + i.leftside.id.value.capitalize + " = " + initialValue
      }
    }
    synchdeclares
  }
  
  def getsynchronizedvariables(ast:List[Definition]):List[Definition]=
  {
    ast.find(x => (x.leftside.description match {case ProgramT() => true; case _=> false})) match
    {
      case None => println("Error in getthreads. Should be caught by checker."); scala.sys.exit()
      case Some(res) => 
      {
        res.rightside.expressions.flatMap(x => x match
        {
          case x:Definition => x.leftside.description match {case SynchronizedVariableT() => Some(x); case _=> None}
          case _=> None
        })
      }
    }
  }
  
}