package fumurtCompiler

import scala.collection.mutable.ListBuffer

object FumurtCodeGenerator
{
  def generate(ast:List[Definition]):String =
  {
    val includestatement = "#include <iostream>\n#include <thread>\n#include <string>\n#include <atomic>\n#include <condition_variable>\n#include <list>\n#include <chrono>\n\n\n"
    val topthreads = gettopthreadstatements(ast)
    val atree = getAnnotatedTree(ast, topthreads)
    println(atree)
    val numtopthreads = topthreads.length
    val synchronizationGlobalVars = "static std::atomic<int> rendezvousCounter;\nstatic std::mutex rendezvousSyncMutex;\nstatic std::condition_variable cv;"
    val main = getmain(topthreads)
    val synchvars = getsynchronizedvariables(ast)
    val syncfunc = getsynchronizerfunction(synchvars, topthreads)
    val synchvardeclarations = getGlobalSynchVariableDeclarations(synchvars)
    val printdecs = getprintlistdeclarations(topthreads)
    //val topthreaddeclarations = gettopthreaddeclarations(ast)
    val (funSignatures, funDeclarations) = getFunctionDeclarations(atree)
    val topThreadNumMacroln = "#define NUMTOPTHREADS " + numtopthreads.toString + "\n"
    
    //println(funSignatures)
    
    includestatement + topThreadNumMacroln + funSignatures + "\n" + synchvardeclarations + printdecs + "\n" + synchronizationGlobalVars + syncfunc + "\n\n" /*+ topthreaddeclarations*/ + "\n"+ funDeclarations + "\n\n" + main
  }
  
  
  def getAnnotatedTree(ast:List[Expression], topthreadcalls:List[FunctionCallStatement]):List[aExpression] = 
  {
    val treeWithAnnotatedDefinitions = getAnnotatedTreeInternal(ast,topthreadcalls,"", None)
    getCallsAnnotatedTreeInternal(treeWithAnnotatedDefinitions, List(), None)
  }
  
  def getCallsAnnotatedTreeInternal(ast:List[aExpression], arguments:List[aDefLhs], containingDefinition:Option[aDefinition]):List[aExpression] =
  {
    val inSameDefinition = indexlefts(ast)
  
    ast.flatMap(node=>node match
      {
        case deff @ aDefinition(aDefLhs(desc,id,cppid,callingthread,args,returntype),aDefRhs(expressions))=>
        {
          val argumentsToDef = args match
          {
            case None => List()
            case Some(Arguments(arglist)) => arglist.flatMap(arg =>
                {
                  val fromargs = arguments.find(x=>x.id.value==arg.id.value)
                  val fromSame = inSameDefinition.find(x=>x.id.value==arg.id.value)
                  fromargs match
                  {
                    case Some(_)=>fromargs
                    case None=>fromSame
                  }
                }
              )
          }
          //println("\n{deff: "+deff+"\nargumentsToDef: "+argumentsToDef+"\nargs: "+args+"\n\nast: "+ast+"\n\narguments: "+arguments+"}\n\n\n")
          val aexpressions = getCallsAnnotatedTreeInternal(expressions,argumentsToDef,Some(deff))
          Some(aDefinition(aDefLhs(desc,id,cppid,callingthread,args,returntype),aDefRhs(aexpressions)))
        }
        case call @ aFunctionCallStatement(fid,_,args,_)=>Some(annotateFunctionCall(call, arguments, inSameDefinition, containingDefinition))
        case z:IdentifierStatement=>Some(z)
        case z:BasicValueStatement=>Some(z)
      }
    )
  }
  
  def annotateFunctionCall(functioncall:aFunctionCallStatement, arguments:List[aDefLhs], inSameDefinition:List[aDefLhs], containingDefinition:Option[aDefinition]):aFunctionCallStatement=
  {
    
    def annotateCallargs(args:Either[aCallarg,aNamedCallargs], arguments:List[aDefLhs], inSameDefinition:List[aDefLhs], containingDefinition:Option[aDefinition]):Either[aCallarg,aNamedCallargs] =
    {
      args match
      {
        case Left(callarg)=>callarg match
        {
          case z:aFunctionCallStatement=>Left(annotateFunctionCall(z, arguments, inSameDefinition, containingDefinition))
          case z:aStatement=>Left(z)
        }
        case Right(aNamedCallargs(callargs))=>Right(aNamedCallargs(callargs.map(namedcallarg=>namedcallarg.argument match
          {
            case z:aFunctionCallStatement=>aNamedCallarg(namedcallarg.id, annotateFunctionCall(z, arguments, inSameDefinition, containingDefinition))
            case aCallarg=>namedcallarg:aNamedCallarg
          }
        )))
      }
    }
    
    val fid = functioncall.functionidentifier
    val args = functioncall.args
    if(fid=="actionPrint" || fid=="toString" || fid=="actionMutate")
    {
      val newargs = annotateCallargs(args, arguments, inSameDefinition, containingDefinition)
      aFunctionCallStatement(fid,fid,newargs,"Nothing")
    }
    else if(fid=="plus" || fid=="minus" || fid=="multiply" || fid=="divide")
    {
      val newargs = annotateCallargs(args, arguments, inSameDefinition, containingDefinition)
      aFunctionCallStatement(fid,fid,newargs,"Number") //TODO: Find actual type like in typechecker. As it is, it only matters if it is Nothing or not.
    }
    else if(fid=="equal" || fid=="lessThan")
    {
      val newargs = annotateCallargs(args, arguments, inSameDefinition, containingDefinition)
      aFunctionCallStatement(fid,fid,newargs,"Boolean")
    }
    else if(fid=="if")
    {
      val newargs = annotateCallargs(args, arguments, inSameDefinition, containingDefinition)
      aFunctionCallStatement(fid,fid,newargs,"Something") //TODO: Find actual type like in typechecker. As it is, it only matters if it is Nothing or not.
    }
    else
    {
      def removeInclusions(args:Either[aCallarg,aNamedCallargs], ldeffargs:Option[Arguments]):Either[aCallarg,aNamedCallargs] = args match
      {
        case Left(callarg)=>
        {
          ldeffargs match
          {
            case Some(Arguments(defargs))=>if(defargs.head.typestr.value=="Inclusion"){Left(NoArgs())}else{args}
            case None=>Left(NoArgs())
            //case _=>Left(NoArgs())
          }
        }
        case Right(aNamedCallargs(namedcallargs))=>
        {
          ldeffargs match
          {
            case Some(Arguments(defargs))=>
            {
              val mnewargs = ListBuffer():ListBuffer[aNamedCallarg]
              for(i<-0 until defargs.length)
              {
                if(defargs(i).typestr.value!="Inclusion")
                {
                  mnewargs += namedcallargs(i)
                }
              }
              Right(aNamedCallargs(mnewargs.toList))
            }
            case None=>println("in getCallsAnnotatedTreeInternal");scala.sys.exit();Left(NoArgs())
            //case _=>Left(NoArgs())
          }
        }
      }
      val ldeff = findinscope(Some(arguments), inSameDefinition, containingDefinition.map(x=>x.leftside), fid)
      val newargs = annotateCallargs(removeInclusions(args, ldeff.args), arguments, inSameDefinition, containingDefinition)
      println("ldeff.cppid.value: "+ldeff.cppid.value)
      aFunctionCallStatement(fid,ldeff.cppid.value,newargs,ldeff.returntype.value)
    }
  }
  
  def indexlefts(in:List[aExpression]):List[aDefLhs]=
  {
    in.foldLeft(List():List[aDefLhs]) ((list,y)=> y match
      { 
        case aDefinition(leftside, _)=>list :+ leftside; 
        case _=> list
      }
    )
  }
  
  def findinscope(arguments:Option[List[aDefLhs]], inSameDefinition:List[aDefLhs], enclosingDefinition:Option[aDefLhs], searchFor:String):aDefLhs=
  {
    val argsres = arguments match{ case Some(args)=>args.filter(y=>y.id.value==searchFor); case None=>List():List[aDefLhs]}
    val inscoperes = inSameDefinition.filter(x=>x.id.value==searchFor)
    
    val enclosingres = enclosingDefinition match
    {
      case None => List()
      case Some(deff) => if (deff.id.value == searchFor) {List(deff)} else {List()}
    }
    
    val res = argsres ++ inscoperes ++ enclosingres
    
    if(res.length==0){println("{arguments: "+arguments+"\n\ninSameDefinition: "+inSameDefinition+"\n\nenclosingDefinition: "+enclosingDefinition+"\n\nsearchFor: "+searchFor+"}\n\n\n")}
    res.head
  }
  
  def getAnnotatedTreeInternal(ast:List[Expression], topthreadcalls:List[FunctionCallStatement], hierarchy:String, callingthread:Option[String]):List[aExpression] =
  {
    val topactions:List[aExpression] = 
    {
      if(hierarchy=="")
      {
        val mess = topthreadcalls.map(threadcall=>threadcall.args match
          {
            case Left(IdentifierStatement(argname))=>
            {
              val deff = ast.filter(x=>x match{case Definition(DefLhs(ActionT(),IdT(thisargname),_,_),_)=>argname==thisargname; case _=>false})
              getAnnotatedTreeInternal(deff,List(), threadcall.functionidentifier, Some(threadcall.functionidentifier)):List[aExpression]
            }
            case Left(_)=> List():List[aExpression]
            case Right(NamedCallargs(namedargs))=>
            {
              val deffs = namedargs.flatMap(namedarg=>
                namedarg match
                {
                  case NamedCallarg(_,IdentifierStatement(argname))=>
                  {
                    ast.find(y=>y match{case Definition(DefLhs(ActionT(),IdT(thisargname),_,_),_)=>argname==thisargname; case _=>false})
                  }
                  case _=>None
                }
              )
              getAnnotatedTreeInternal(deffs,List(), threadcall.functionidentifier, Some(threadcall.functionidentifier)):List[aExpression]
            }
          }
        ):List[List[aExpression]]
        
        mess.foldLeft(List():List[aExpression])((x,y)=>x++y):List[aExpression]
      }
      else
      {
        List()
      }
    }
    val rest:List[aExpression] = ast.flatMap(x=>x match
      {
        case Definition(DefLhs(ThreadT(),id,args,returntype),DefRhs(expressions)) => 
        {
          val aexps = getAnnotatedTreeInternal(expressions, topthreadcalls.filter(x=>x.functionidentifier==id.value), id.value, Some(id.value))
          Some(aDefinition(aDefLhs(ThreadT(),id,id,id.value,args,returntype),aDefRhs(aexps)))
        }
        
        case Definition(DefLhs(FunctionT(),id,args,returntype),DefRhs(expressions)) => 
        {
          val aexps = getAnnotatedTreeInternal(expressions, topthreadcalls, hierarchy+id.value, callingthread)
          Some(aDefinition(aDefLhs(FunctionT(),id,IdT(id.value+"$"+hierarchy),"shouldn't matter",args,returntype),aDefRhs(aexps)))
        }
        case Definition(DefLhs(ProgramT(),_,_,_),_) => None //we don't really care about it...
        case Definition(DefLhs(ActionT(),id,args,returntype),DefRhs(expressions)) => 
        {
          if(hierarchy=="")
          {
            None
          }
          else
          {
            val aexps = getAnnotatedTreeInternal(expressions, topthreadcalls, hierarchy+id.value, callingthread)
            Some(aDefinition(aDefLhs(ActionT(),id,IdT(id.value+"$"+hierarchy),callingthread match{case Some(z)=>z; case None=>"not found"},args,returntype),aDefRhs(aexps)))
          }
        }
        case FunctionCallStatement(fid,args)=>
        {
          def annotateCallarg(callarg:Callarg):aCallarg=
          {
            callarg match
            { 
              case z:aCallarg => z
              case FunctionCallStatement(fid,args)=>
              {
                val newargs:Either[aCallarg,aNamedCallargs] = args match
                {
                  case Left(arg)=>Left(annotateCallarg(arg))
                  case Right(NamedCallargs(arglist))=>Right(aNamedCallargs(arglist.map(x=>aNamedCallarg(x.id,annotateCallarg(x.argument)) )))
                }
                aFunctionCallStatement(fid,"not filled out",newargs,"not filled out")
              }
            }
          }
          Some(annotateCallarg(FunctionCallStatement(fid,args))):Option[aExpression]
        }
        case z:IdentifierStatement=>Some(z)
      }
    ):List[aExpression]
    rest++topactions
    
  }
  
  def getFunctionDeclarations(ast:List[aExpression]):(String,String) =
  {
    def actfunrecursivetranslate(cppid:IdT, callingthread:String, args:Option[Arguments], returntype:TypeT, expressions:List[aExpression]) =
    {
      val signature = getFunctionSignature(cppid, args, returntype)
      val functionstart = signature+"\n{"
      val functionend = "\n}\n"
      val generals = expressions.flatMap(
        y=> y match
        {
          case aDefinition(leftside, rightside)=>None
          case z:aFunctionCallStatement =>
          {
            if(z.returntype!="Nothing")
            {
              Some("return "+functioncalltranslator(z, callingthread) + "; //returntype: "+z.returntype)
            }
            else
            {
              Some(functioncalltranslator(z, callingthread) + ";")
            }
            
          }
          case IdentifierStatement(value) => Some("return "+value+";")
          case StringStatement(value) => Some("return "+value+";")
          case IntegerStatement(value) => Some("return "+value.toString+";")
          case DoubleStatement(value) => Some("return "+value.toString+";")
          case TrueStatement() => Some("return true;")
          case FalseStatement() => Some("return false;")
          //case _=> "not implemented" //println("Error in gettopthreaddeclarations. Not implemented."); scala.sys.exit()
        }
      ).foldLeft("")((x,y)=>x+"\n  "+y)
      val underfunctions = getFunctionDeclarations(expressions)
      val body = functionstart+generals+functionend
      //Some((signature+";",body))
      Some((signature+";"+underfunctions._1, body+underfunctions._2))
    }
    
    val list = ast.flatMap(node=>node match
      {
        case aDefinition(aDefLhs(ThreadT(),id,cppid,_,_,_),aDefRhs(expressions))=>
        {
          val signature = "[[noreturn]] static void "+cppid.value+"()"
          val functionstart = signature+"\n{"
          val functionend = "\n}\n"
          val (tailrecursestart, tailrecurseend) = ("while(true)\n{", "\n}")
          val generals = expressions.flatMap(
            y=> y match
            {
              case aDefinition(leftside, rightside)=>None
              case aFunctionCallStatement(id.value,_, args,_) => Some("waitForRendezvous(\""+cppid.value+"\");\n  continue;")
              case z:aFunctionCallStatement => Some(functioncalltranslator(z, id.value) + ";")
              //case _=> "not implemented" //println("Error in gettopthreaddeclarations. Not implemented."); scala.sys.exit()
            }
          ).foldLeft("")((x,y)=>x+"\n  "+y)
          val underfunctions = getFunctionDeclarations(expressions)
          val body = functionstart+tailrecursestart+generals+tailrecurseend+functionend
          Some((signature+";"+underfunctions._1, body+underfunctions._2))
          
        }
        case z:aFunctionCallStatement=>None
        case z:IdentifierStatement=>None
        case aDefinition(aDefLhs(ActionT(),id,cppid,callingthread,args,returntype),aDefRhs(expressions))=>actfunrecursivetranslate(cppid, callingthread, args, returntype, expressions)
        case aDefinition(aDefLhs(FunctionT(),id,cppid,callingthread,args,returntype),aDefRhs(expressions))=>actfunrecursivetranslate(cppid, callingthread, args, returntype, expressions)
      }
    ):List[(String,String)]
    list.foldLeft(("",""))((x,y)=>(x._1+"\n"+y._1,x._2+"\n"+y._2))
  }
  
  
  
  
  def getFunctionSignature(cppid:IdT, optargs:Option[Arguments], returntype:TypeT):String =
  {
    def argtranslator(arg:Argument):String=
    {
      typetranslator(arg.typestr)+" "+arg.id.value
    }
    val argsString = optargs match
    {
      case None=>""
      case Some(Arguments(List(arg)))=>
      {
        if(arg.typestr.value!="Inclusion")
        {
          argtranslator(arg)
        }
        else{""}
      }
      case Some(Arguments(args))=>argtranslator(args.head) + args.tail.foldLeft("")((x,y)=>
        if(y.typestr.value!="Inclusion"){x+", "+argtranslator(y)} else{x}
      )
      
    }
  
    typetranslator(returntype)+" "+cppid.value+"("+argsString+")"
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
      case "Boolean"=>"bool"
      case _=>"not implemented"
    }
  }
  
  def callargTranslator(callarg:aCallarg, callingthread:String):String =
  {
    callarg match
    {
      case StringStatement(value)=>value
      case IntegerStatement(value)=>value.toString
      case DoubleStatement(value)=>value.toString
      case TrueStatement()=>"true"
      case FalseStatement()=>"false"
      case IdentifierStatement(value)=>value
      case call:aFunctionCallStatement=>functioncalltranslator(call:aFunctionCallStatement, callingthread:String)
      case NoArgs()=>""
    }
  }
  
  def functioncalltranslator(call:aFunctionCallStatement, callingthread:String):String =
  {
    //println("in functioncalltranslator. call is "+call)
    //if(call.functionidentifier=="plus"){println("found")}
    println("\n\n"+call)
    call match
    {
      case aFunctionCallStatement("actionPrint",_, Left(StringStatement(value)),_) => "print" + callingthread + ".push_back(" + value + ")"
      case aFunctionCallStatement("actionPrint",_, Left(IdentifierStatement(value)),_) => "print" + callingthread + ".push_back(std::to_string(" + value + "))"
      case aFunctionCallStatement("actionPrint",_, Left(x:aFunctionCallStatement),_) => "print" + callingthread + ".push_back(" + functioncalltranslator(x,callingthread) + ")"
      case aFunctionCallStatement("toString",_, Left(x:aFunctionCallStatement),_) => "std::to_string(" + functioncalltranslator(x,callingthread) + ")"
      case aFunctionCallStatement("toString",_, Left(IdentifierStatement(value)),_) => "std::to_string(" + value + ")"
      case aFunctionCallStatement("toString",_, Left(IntegerStatement(value)),_) => "std::to_string(" + value.toString + ")"
      case aFunctionCallStatement("toString",_, Left(DoubleStatement(value)),_) => "std::to_string(" + value.toString + ")"
      case aFunctionCallStatement("toString",_, Left(TrueStatement()),_) => "true"
      case aFunctionCallStatement("toString",_, Left(FalseStatement()),_) => "false"
      case aFunctionCallStatement("equal",_,Right(aNamedCallargs(List(aNamedCallarg(IdT("left"),IntegerStatement(left)), aNamedCallarg(IdT("right"),IntegerStatement(right))))),_) => left.toString+" == "+right.toString
      case aFunctionCallStatement("lessThan",_,Right(aNamedCallargs(List(aNamedCallarg(IdT("left"),IntegerStatement(left)), aNamedCallarg(IdT("right"),IntegerStatement(right))))),_) => left.toString+" < "+right.toString
      //TODO: add more types that the comparison functions can accept
      case aFunctionCallStatement("actionMutate",_, Right(aNamedCallargs(List(aNamedCallarg(IdT("newValue"),IdentifierStatement(newval)), aNamedCallarg(IdT("variable"),IdentifierStatement(vari))))),_) => vari + " = " + newval
      case aFunctionCallStatement("actionMutate",_, Right(aNamedCallargs(List(aNamedCallarg(IdT("newValue"),x:aFunctionCallStatement), aNamedCallarg(IdT("variable"),IdentifierStatement(vari))))),_) =>
      {
        "write" + vari.capitalize + " = " + functioncalltranslator(x, callingthread)
      }
      case aFunctionCallStatement("plus",_,_,_) => basicmathcalltranslator(call, callingthread)
      case aFunctionCallStatement("minus",_,_,_) => basicmathcalltranslator(call, callingthread)
      case aFunctionCallStatement("multiply",_,_,_) => basicmathcalltranslator(call, callingthread)
      case aFunctionCallStatement("divide",_,_,_) => basicmathcalltranslator(call, callingthread)
      case aFunctionCallStatement("if",_, Right(aNamedCallargs(List(aNamedCallarg(IdT("condition"),condstat), aNamedCallarg(IdT("else"),elsestat), aNamedCallarg(IdT("then"),thenstat)))),_) => 
      {
        def translator(in:aStatement):String=
        {
          in match
          {
            case TrueStatement()=>"true"
            case FalseStatement()=>"false"
            case StringStatement(value)=>value
            case IntegerStatement(value)=>value.toString
            case DoubleStatement(value)=>value.toString
            case IdentifierStatement(value)=>value //Correct behaviour? .....
            case z:aFunctionCallStatement=>functioncalltranslator(z, callingthread)
          }
        }
        condstat match
        {
          case TrueStatement()=>translator(thenstat)
          case FalseStatement()=>translator(elsestat)
          case _=> 
          {
            translator(condstat)+" ? "+translator(thenstat)+" : "+translator(elsestat)
          }
        }
      }
      //case aFunctionCallStatement("equals",_, Right(aNamedCallargs(List(aNamedCallarg(IdT("left"),), aNamedCallarg(IdT("right"),)))),_)
      case aFunctionCallStatement(funcid,cppfuncid,args,_) =>
      {
        val argstr = args match
          {
            case Left(callarg)=>callargTranslator(callarg, callingthread)
            case Right(aNamedCallargs(args)) =>
            {
              val first = callargTranslator(args.head.argument, callingthread)
              val subsequent = args.foldLeft("")((x,y)=>x+", "+callargTranslator(y.argument, callingthread))
              first+subsequent
            }
          }
        cppfuncid+"("+argstr+")"
      }
      case _=> "not implemented"
    }
  }
  
  def basicmathcalltranslator(call:aFunctionCallStatement, callingthread:String):String=
  {
    val operator = if(call.functionidentifier=="plus"){" + "}else if(call.functionidentifier=="minus"){" - "}else if(call.functionidentifier=="multiply"){" * "}else if(call.functionidentifier=="minus"){" / "}
    call match
    {
      //case FunctionCallStatement(_, Right(NamedCallargs(List(NamedCallarg(IdT("left"),IdentifierStatement(left)), NamedCallarg(IdT("right"),IdentifierStatement(right)))))) => left + operator + right
      //case FunctionCallStatement(_, Right(NamedCallargs(List(NamedCallarg(IdT("left"),IdentifierStatement(left)), NamedCallarg(IdT("right"),IntegerStatement(right)))))) => left + operator + right
      //case FunctionCallStatement(_, Right(NamedCallargs(List(NamedCallarg(IdT("left"),IdentifierStatement(left)), NamedCallarg(IdT("right"),DoubleStatement(right)))))) => left + operator + right
      case aFunctionCallStatement(_,_, Right(aNamedCallargs(callargs)),_) =>
      {
        val argstr = callargs.map(arg=>
          {
            arg match
            {
              case aNamedCallarg(_,IdentifierStatement(value))=>value
              case aNamedCallarg(_,IntegerStatement(value))=>value.toString
              case aNamedCallarg(_,DoubleStatement(value))=>value.toString
              case aNamedCallarg(_,f:aFunctionCallStatement)=>functioncalltranslator(f, callingthread)
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






trait aExpression
trait aCallarg extends Callarg with aStatement
trait aStatement extends aExpression

case class aDefinition(val leftside:aDefLhs, val rightside:aDefRhs) extends aExpression
case class aDefLhs(val description:DefDescriptionT, val id:IdT, val cppid:IdT, val callingthread:String, val args:Option[Arguments], val returntype:TypeT)
//case class Arguments(val args:List[Argument])
//case class Argument(val id:IdT, val typestr:TypeT)
case class aDefRhs(val expressions:List[aExpression] )
case class aNamedCallarg(id:IdT, argument:aCallarg) //extends Callarg
case class aNamedCallargs(val value:List[aNamedCallarg])

case class aFunctionCallStatement(val functionidentifier:String, val cppfunctionidentifier:String, val args:Either[aCallarg,aNamedCallargs], val returntype:String) extends aStatement with aCallarg










