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
    val syncfunc = getsynchronizerfunction(synchvars, topthreads)
    val synchvardeclarations = getGlobalSynchVariableDeclarations(synchvars)
    val printdecs = getprintlistdeclarations(topthreads)
    val topthreaddeclarations = gettopthreaddeclarations(ast)
    
    includestatement + "#define NUMTOPTHREADS " + numtopthreads.toString + "\n" + synchvardeclarations + printdecs + "\n" + synchronizationGlobalVars + syncfunc + "\n\n" + topthreaddeclarations + "\n" + main
  }
  
  def gettopthreaddeclarations(ast:List[Definition]):String =
  {
    ast.filter(x => (x.leftside.description match {case ThreadT() => true; case _=> false})).map(x=>
      {   
        val functionstart = "[[noreturn]] static void " + x.leftside.id.value + "()\n{"
        val functionend = "\n}\n"
        val (tailrecursestart, tailrecurseend) = ("while(true)\n{", "\n}") /*x.rightside.expressions.last match
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
        }*/
        val generals = x.rightside.expressions.flatMap(
          y=> y match
          {
            //case FunctionCallStatement("println", args) => "print" + x.leftside.id.value + ".push_back(" + args match{case Left(Callarg) =>; case Right(_)=>"not implemented"} + ");"
            //case FunctionCallStatement("println", Left(StringStatement(value))) => "print" + x.leftside.id.value + ".push_back(" + value + ");"
            //case FunctionCallStatement("println", Left(IdentifierStatement(value))) => "print" + x.leftside.id.value + ".push_back(" + value + ");"
            //case FunctionCallStatement("mutate", Right(NamedCallargs(List(NamedCallarg(IdT("newValue"),IdentifierStatement(newval)), NamedCallarg(IdT("variable"),IdentifierStatement(vari)))))) => vari + "=" + newval + ";"
            case Definition(leftside, rightside)=>None
            case FunctionCallStatement(x.leftside.id.value, args) => Some("waitForRendezvous(\""+x.leftside.id.value+"\");\n  continue;")
            case z:FunctionCallStatement => Some(functioncalltranslator(z, x.leftside.id.value) + ";")
            //case _=> "not implemented" //println("Error in gettopthreaddeclarations. Not implemented."); scala.sys.exit()
          }
        ).foldLeft("")((x,y)=>x+"\n  "+y)
        functionstart + tailrecursestart + generals + tailrecurseend + functionend
      }
    ).foldLeft("")((x,y)=>x+y)
    
  }
  
  def getFunctionDeclarations(ast:List[Expression]):(String,String) =
  {
    ast.flatMap(x => x match
      {
        case z:Expression=>None
        case Definition(DefLhs(ThreadT(),_,_,_),DefRhs(expressions))=>Some(getFunctionDeclarations(expressions))
        case Definition(DefLhs(FunctionT(),_,_,_),_)=>None //TODO
        case Definition(DefLhs(ActionT(),_,_,_),_)=>Some()
      }
    ).map(x=>x.foldLeft("")((x,y)=>
        x+"\n  "+y
      )
    )
  }
  
  def functioncalltranslator(call:FunctionCallStatement, callingthread:String):String =
  {
    //println("in functioncalltranslator. call is "+call)
    //if(call.functionidentifier=="plus"){println("found")}
    
    call match
    {
      case FunctionCallStatement("actionPrint", Left(StringStatement(value))) => "print" + callingthread + ".push_back(" + value + ")"
      case FunctionCallStatement("actionPrint", Left(IdentifierStatement(value))) => "print" + callingthread + ".push_back(std::to_string(" + value + "))"
      case FunctionCallStatement("actionPrint", Left(x:FunctionCallStatement)) => "print" + callingthread + ".push_back(" + functioncalltranslator(x,callingthread) + ")"
      case FunctionCallStatement("toString", Left(x:FunctionCallStatement)) => "std::to_string(" + functioncalltranslator(x,callingthread) + ")"
      case FunctionCallStatement("toString", Left(IdentifierStatement(value))) => "std::to_string(" + value + ")"
      case FunctionCallStatement("actionMutate", Right(NamedCallargs(List(NamedCallarg(IdT("newValue"),IdentifierStatement(newval)), NamedCallarg(IdT("variable"),IdentifierStatement(vari)))))) => vari + " = " + newval
      case FunctionCallStatement("actionMutate", Right(NamedCallargs(List(NamedCallarg(IdT("newValue"),x:FunctionCallStatement), NamedCallarg(IdT("variable"),IdentifierStatement(vari)))))) =>
      {
        "write" + vari.capitalize + " = " + functioncalltranslator(x, callingthread)
      }
      case FunctionCallStatement("plus",_) => basicmathcalltranslator(call, callingthread)
      case FunctionCallStatement("minus",_) => basicmathcalltranslator(call, callingthread)
      case FunctionCallStatement("multiply",_) => basicmathcalltranslator(call, callingthread)
      case FunctionCallStatement("divide",_) => basicmathcalltranslator(call, callingthread)
      case _=> "not implemented"
    }
  }
  
  def basicmathcalltranslator(call:FunctionCallStatement, callingthread:String):String=
  {
    val operator = if(call.functionidentifier=="plus"){" + "}else if(call.functionidentifier=="minus"){" - "}else if(call.functionidentifier=="multiply"){" * "}else if(call.functionidentifier=="minus"){" / "}
    call match
    {
      //case FunctionCallStatement(_, Right(NamedCallargs(List(NamedCallarg(IdT("left"),IdentifierStatement(left)), NamedCallarg(IdT("right"),IdentifierStatement(right)))))) => left + operator + right
      //case FunctionCallStatement(_, Right(NamedCallargs(List(NamedCallarg(IdT("left"),IdentifierStatement(left)), NamedCallarg(IdT("right"),IntegerStatement(right)))))) => left + operator + right
      //case FunctionCallStatement(_, Right(NamedCallargs(List(NamedCallarg(IdT("left"),IdentifierStatement(left)), NamedCallarg(IdT("right"),DoubleStatement(right)))))) => left + operator + right
      case FunctionCallStatement(_, Right(NamedCallargs(callargs))) =>
      {
        val argstr = callargs.map(arg=>
          {
            arg match
            {
              case NamedCallarg(_,IdentifierStatement(value))=>value
              case NamedCallarg(_,IntegerStatement(value))=>value.toString
              case NamedCallarg(_,DoubleStatement(value))=>value.toString
              case NamedCallarg(_,f:FunctionCallStatement)=>functioncalltranslator(f, callingthread)
            }
          }
        ):List[String]
        "(" + argstr(0) + operator + argstr(1) + ")"
      } 
    }
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
  
  def getsynchronizerfunction(synchvariables:List[Definition], topthreads:List[FunctionCallStatement]):String=
  {
    var synchvariablestrings = ""
    
    for(i<-synchvariables)
    {
      val name = i.leftside.id.value
      synchvariablestrings += name + " = write" + name.capitalize + ";\n"
    }
    
    var printstatements = ""
    for(i<-topthreads)
    {
      var currentprintqueuename = "print" + i.functionidentifier
      printstatements += "while(!"+currentprintqueuename+".empty()){\nstd::cout << "+currentprintqueuename+".front();\n"+currentprintqueuename+".pop_front();\n}\n"
    }
    
    ("""static void waitForRendezvous(std::string name)
{
  std::unique_lock<std::mutex> lk(rendezvousSyncMutex);
  ++rendezvousCounter;
  if(rendezvousCounter.load() < NUMTOPTHREADS)
  {
    cv.wait(lk);
  }
  else if (rendezvousCounter.load() == NUMTOPTHREADS)
  {
    """
     + printstatements + synchvariablestrings + """
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
}""")
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
            //case DoubleStatement(value) => value
            case _=> println("Error in getGlobalSynchVariableDeclarations. Should be caught by checker."); scala.sys.exit()
          }
          case _=> println("Error in getGlobalSynchVariableDeclarations. Should be caught by checker."); scala.sys.exit()
        }
        case _=> println(i.rightside.expressions(0).toString); println("Error in getGlobalSynchVariableDeclarations. Should be caught by checker."); scala.sys.exit()
      }
      if (fumurttype == "Integer")
      {
        synchdeclares += "\nstatic int " + i.leftside.id.value + " = " + initialValue + ";"
        synchdeclares += "\nstatic int write" + i.leftside.id.value.capitalize + " = " + initialValue + ";"
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
