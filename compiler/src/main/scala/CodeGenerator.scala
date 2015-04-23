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
    val (funSignatures, funDeclarations) = getFunctionDeclarations(ast, topthreads)
    val topThreadNumMacroln = "#define NUMTOPTHREADS " + numtopthreads.toString + "\n"
    
    println(funSignatures)
    
    includestatement + topThreadNumMacroln + funSignatures + synchvardeclarations + printdecs + "\n" + synchronizationGlobalVars + syncfunc + "\n\n" + topthreaddeclarations + "\n"+ funDeclarations + "\n\n" + main
  }
  
  def gettopthreaddeclarations(ast:List[Definition]):String =
  {
    val topactfuns = ast.filter(x => (x.leftside.description match {case ActionT() => true; case FunctionT()=>true; case _=> false}))
    
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
        val inclusions = x.leftside.args match
        {
          case None=>List()
          case Some(Arguments(args))=>args.flatMap(y=>
          if(y.typestr.value=="Inclusion")
          {
            topactfuns.find(z=>if(z.leftside.id.value==y.id.value){true}else{false})
          }
          else
          {
            None
          }
          )
        }
        val generals = x.rightside.expressions.flatMap(
          y=> y match
          {
            //case FunctionCallStatement("println", args) => "print" + x.leftside.id.value + ".push_back(" + args match{case Left(Callarg) =>; case Right(_)=>"not implemented"} + ");"
            //case FunctionCallStatement("println", Left(StringStatement(value))) => "print" + x.leftside.id.value + ".push_back(" + value + ");"
            //case FunctionCallStatement("println", Left(IdentifierStatement(value))) => "print" + x.leftside.id.value + ".push_back(" + value + ");"
            //case FunctionCallStatement("mutate", Right(NamedCallargs(List(NamedCallarg(IdT("newValue"),IdentifierStatement(newval)), NamedCallarg(IdT("variable"),IdentifierStatement(vari)))))) => vari + "=" + newval + ";"
            case Definition(leftside, rightside)=>None
            case FunctionCallStatement(x.leftside.id.value, args) => Some("waitForRendezvous(\""+x.leftside.id.value+"\");\n  continue;")
            case z:FunctionCallStatement => 
            {
              val fundef = inclusions.find(v=>if(z.functionidentifier==v.leftside.id.value){true}else{false})
              val suffix = fundef match
              {
                case None=>""
                case Some(Definition(DefLhs(ActionT(),_,_,_),_))=>x.leftside.id.value
                case Some(Definition(DefLhs(FunctionT(),_,_,_),_))=>""
              }
              Some(functioncalltranslator(z, x.leftside.id.value, suffix, fundef) + ";")
            }
            //case _=> "not implemented" //println("Error in gettopthreaddeclarations. Not implemented."); scala.sys.exit()
          }
        ).foldLeft("")((x,y)=>x+"\n  "+y)
        functionstart + tailrecursestart + generals + tailrecurseend + functionend
      }
    ).foldLeft("")((x,y)=>x+y)
    
  }
  
  def getFunctionDeclarations(ast:List[Expression], topthreadcalls:List[FunctionCallStatement]):(String,String) =
  {
      def getDeclarations(ast:List[Expression], hierarchy:String, callingthread:String, defdesc:DefDescriptionT):(String,String) =
      {
        ast.flatMap(x => x match
          {
            case z:Statement=>None
            //case Definition(DefLhs(ThreadT(),id,_,_),DefRhs(expressions))=>Some(getFunctionDeclarations(expressions, hierarchy+id.value))
            case Definition(DefLhs(thisdefdesc,id,args,returntype),DefRhs(expressions))=>
            {
            //if(thisdefdesc.toString!=defdesc.toString){None}
              if(thisdefdesc.toString==defdesc.toString)
              {
                val signature = getFunctionSignature(id, args, returntype, hierarchy)
                val generals = expressions.flatMap(
                  y=>y match
                  {
                    case Definition(leftside, rightside)=>None
                    case z:FunctionCallStatement => Some(functioncalltranslator(z, callingthread) + ";") //id.value is not calling thread
                    //TODO: Add support for returning stuff
                  }
                ).foldLeft("")((x,y)=>x+"\n  "+y)
                val (furtherFunSignatures, furtherFunBodies) = getDeclarations(expressions, hierarchy+id.value, callingthread, FunctionT())
                val (furtherActSignatures, furtherActBodies) = getDeclarations(expressions, hierarchy+id.value, callingthread, ActionT())
                Some((signature+";\n"+furtherFunSignatures+furtherActSignatures, signature+"\n{"+ generals +"\n}\n"+furtherFunBodies+furtherActBodies))
              }
              else{None}
            }
            case _=>None
          }
        ).foldLeft(("",""))((x,y)=>(if(y._1!=""){x._1+y._1}else{x._1},       x._2+"\n  "+y._2))
      }
  
    //val topthreaddefs = ast.filter(x=>x match{case Definition(DefLhs(ThreadT(),_,_,_),_)=>true; case _=>false})
    val (topActionSignatures, topActionBodies) = topthreadcalls.map(threadcall=>
      threadcall.args match
      {
        case Left(IdentifierStatement(argname))=>
        {
          val action = ast.filter(x=>x match{case Definition(DefLhs(ActionT(),IdT(thisargname),_,_),_)=>if(argname==thisargname){true}else{false}; case _=>false})
          getDeclarations(action, threadcall.functionidentifier, threadcall.functionidentifier, ActionT())
        }
        case Left(_)=>("","")
        case Right(NamedCallargs(namedargs))=>
        {
          val actions = namedargs.flatMap(x=>
            x match
            {
              case NamedCallarg(_,IdentifierStatement(argname))=>
              {
                ast.find(y=>y match{case Definition(DefLhs(ActionT(),IdT(thisargname),_,_),_)=>if(argname==thisargname){true}else{false}; case _=>false}) match
                {
                  case None=>None
                  case Some(z)=>Some(z)
                }
              }
              case _=>None
            }
          )
          getDeclarations(actions, threadcall.functionidentifier, threadcall.functionidentifier, ActionT())
        }
      }
    ).foldLeft(("",""))((x,y)=>(x._1+y._1, x._2+y._2)) :(String,String)
    
    val (topFunctionSignatures, topFunctionBodies) = getDeclarations(ast, "", "", FunctionT()):(String,String)
    val (threadInternalSignatures, threadInternalBodies) = ast.flatMap(x=>x match
      {
        case Definition(DefLhs(ThreadT(),id,_,_),DefRhs(expressions))=>
        {
          val acts = getDeclarations(expressions, id.value, id.value, ActionT()):(String,String)
          val funs = getDeclarations(expressions, id.value, id.value, FunctionT()):(String,String)
          Some((acts._1+funs._1, acts._2+funs._2))
        }
        
        case _=>None
      }
    ).foldLeft(("",""))((x,y)=>(x._1+y._1, x._2+y._2)) :(String,String)
    (topActionSignatures+topFunctionSignatures+threadInternalSignatures, topActionBodies+topFunctionBodies+threadInternalBodies)
  }
  
  
  
  def getFunctionSignature(id:IdT, optargs:Option[Arguments], returntype:TypeT, hierarchy:String):String =
  {
    def argtranslator(arg:Argument):String=
    {
      typetranslator(arg.typestr)+" "+arg.id.value
    }
    val argsString = optargs match
    {
      case None=>""
      case Some(Arguments(List(arg)))=>argtranslator(arg)
      case Some(Arguments(args))=>argtranslator(args.head) + args.tail.foldLeft("")((x,y)=>
        if(y.typestr.value!="Inclusion"){x+", "+argtranslator(y)} else{x}
      )
      
    }
  
    typetranslator(returntype)+" "+id.value+"$"+hierarchy+"("+argsString+")"
  }
  
  def typetranslator(in:TypeT):String =
  {
    in.value match
    {
      case "Integer"=>"int"
      case "Double"=>"double"
      case "String"=>"std::string"
      case "Nothing"=>"void"
      case "Inclusion"=>"shouldn't be here"
      case _=>"not implemented"
    }
  }
  
  def functioncalltranslator(call:FunctionCallStatement, callingthread:String, suffix:String, funcdef:Option[Definition]):String =
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
      case FunctionCallStatement(funcid,args) => funcdef match
      {
        case None=>"everything's wrong"
        case Some(sfuncdef)=>
        {
          
          funcid+"$"+suffix+"("+  +")"
        }
      }
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
