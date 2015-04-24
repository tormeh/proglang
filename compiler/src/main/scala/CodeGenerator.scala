package fumurtCompiler


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
    
    println(funSignatures)
    
    includestatement + topThreadNumMacroln + funSignatures + synchvardeclarations + printdecs + "\n" + synchronizationGlobalVars + syncfunc + "\n\n" /*+ topthreaddeclarations*/ + "\n"+ funDeclarations + "\n\n" + main
  }
  
  /*def gettopthreaddeclarations(ast:List[Definition]):String =
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
        def getGenericFunctionCall(thread:Definition, inclusions:List[Definition], functionid:String, arguments:Either[Callarg,NamedCallargs]):String =
        {
          val fundef = inclusions.find(v=>if(functionid==v.leftside.id.value){true}else{false})
          val suffix:String = fundef match
          {
            case None=>thread.leftside.id.value
            case Some(Definition(DefLhs(ActionT(),_,_,_),_))=>thread.leftside.id.value
            case Some(Definition(DefLhs(FunctionT(),_,_,_),_))=>""
            case Some(_)=>"bug bug bug"
          }
          val argstr = arguments match
          {
            case Left(callarg)=>callargTranslator(callarg, getGenericFunctionCall(thread, inclusions,_:String,_:Either[Callarg,NamedCallargs]))
            case Right(NamedCallargs(args)) =>
            {
              val first = callargTranslator(args.head.argument, getGenericFunctionCall(thread, inclusions,_:String,_:Either[Callarg,NamedCallargs]))
              val subsequent = args.foldLeft("")((x,y)=>x+", "+callargTranslator(y.argument, getGenericFunctionCall(thread, inclusions,_:String,_:Either[Callarg,NamedCallargs])))
              first+subsequent
            }
          }
          functionid+"$"+suffix+"("+argstr+")"
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
            case z:FunctionCallStatement => Some(functioncalltranslator(z, x.leftside.id.value, getGenericFunctionCall(x,inclusions,_:String,_:Either[Callarg,NamedCallargs])) + ";")
            //case _=> "not implemented" //println("Error in gettopthreaddeclarations. Not implemented."); scala.sys.exit()
          }
        ).foldLeft("")((x,y)=>x+"\n  "+y)
        functionstart + tailrecursestart + generals + tailrecurseend + functionend
      }
    ).foldLeft("")((x,y)=>x+y)
    
  }*/
  
  def getAnnotatedTree(ast:List[Expression], topthreadcalls:List[FunctionCallStatement]):List[aExpression] = 
  {
    val treeWithAnnotatedDefinitions = getAnnotatedTreeInternal(ast,topthreadcalls,"")
    getCallsAnnotatedTreeInternal(treeWithAnnotatedDefinitions, List(), None)
  }
  
  def getCallsAnnotatedTreeInternal(ast:List[aExpression], arguments:List[aDefLhs], containingDefinition:Option[aDefinition]):List[aExpression] =
  {
    val inSameDefinition = indexlefts(ast)
  
    ast.flatMap(node=>node match
      {
        case deff @ aDefinition(aDefLhs(desc,id,cppid,args,returntype),aDefRhs(expressions))=>
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
          Some(aDefinition(aDefLhs(desc,id,cppid,args,returntype),aDefRhs(aexpressions)))
        }
        case aFunctionCallStatement(fid,cppfid,args)=>
        {
          if(fid=="actionPrint" || fid=="actionMutate" || fid=="plus" || fid=="minus" || fid=="multiply" || fid=="divide")
          {
            Some(aFunctionCallStatement(fid,fid,args))
          }
          else
          {
            val ldeff = findinscope(Some(arguments), inSameDefinition, containingDefinition.map(x=>x.leftside), fid)
            Some(aFunctionCallStatement(fid,ldeff.cppid.value,args))
          }
        }
      }
    )
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
  
  def getAnnotatedTreeInternal(ast:List[Expression], topthreadcalls:List[FunctionCallStatement], hierarchy:String):List[aExpression] =
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
              getAnnotatedTreeInternal(deff,List(), threadcall.functionidentifier):List[aExpression]
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
              getAnnotatedTreeInternal(deffs,List(), threadcall.functionidentifier):List[aExpression]
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
          val aexps = getAnnotatedTreeInternal(expressions, topthreadcalls.filter(x=>x.functionidentifier==id.value), id.value)
          Some(aDefinition(aDefLhs(ThreadT(),id,id,args,returntype),aDefRhs(aexps)))
        }
        
        case Definition(DefLhs(FunctionT(),id,args,returntype),DefRhs(expressions)) => 
        {
          val aexps = getAnnotatedTreeInternal(expressions, topthreadcalls, hierarchy+id.value)
          Some(aDefinition(aDefLhs(FunctionT(),id,IdT(hierarchy+"$"+id.value),args,returntype),aDefRhs(aexps)))
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
            val aexps = getAnnotatedTreeInternal(expressions, topthreadcalls, hierarchy+id.value)
            Some(aDefinition(aDefLhs(FunctionT(),id,IdT(hierarchy+"$"+id.value),args,returntype),aDefRhs(aexps)))
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
                aFunctionCallStatement(fid,"not filled out",newargs)
              }
            }
          }
          Some(annotateCallarg(FunctionCallStatement(fid,args))):Option[aExpression]
        }
      }
    ):List[aExpression]
    rest++topactions
    
  }
  
  def getFunctionDeclarations(ast:List[aExpression]):(String,String) =
  {
    val list = ast.flatMap(node=>node match
      {
        case aDefinition(aDefLhs(ThreadT(),id,cppid,_,_),aDefRhs(expressions))=>
        {
          val functionstart = "[[noreturn]] static void " + cppid + "()\n{"
          val functionend = "\n}\n"
          val (tailrecursestart, tailrecurseend) = ("while(true)\n{", "\n}")
          val generals = expressions.flatMap(
            y=> y match
            {
              case aDefinition(leftside, rightside)=>None
              case aFunctionCallStatement(id.value,_, args) => Some("waitForRendezvous(\""+cppid+"\");\n  continue;")
              case z:aFunctionCallStatement => Some(functioncalltranslator(z, id.value) + ";")
              //case _=> "not implemented" //println("Error in gettopthreaddeclarations. Not implemented."); scala.sys.exit()
            }
          ).foldLeft("")((x,y)=>x+"\n  "+y)
          val underfunctions = getFunctionDeclarations(expressions)
          val signature = "static void "+cppid+"();\n"
          val body = functionstart+tailrecursestart+generals+tailrecurseend+functionend
          Some((signature+underfunctions._1, body+underfunctions._2))
          
        }
        case z:aFunctionCallStatement=>None
      }
    ):List[(String,String)]
    list.foldLeft(("",""))((x,y)=>(x._1+"\n"+y._1,x._2+"\n"+y._2))
  }
  
  /*def getFunctionDeclarations(ast:List[Expression], topthreadcalls:List[FunctionCallStatement]):(String,String) =
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
                    case z:FunctionCallStatement => Some(functioncalltranslator(z, callingthread, ((l:String,r:Either[Callarg,NamedCallargs])=>"not implemented")) + ";") //TODO
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
  }*/
  
  
  
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
  /*
  def callargTranslator(callarg:Callarg, getCall:((String,Either[Callarg,NamedCallargs])=>String)):String =
  {
    callarg match
    {
      case StringStatement(value)=>value
      case IntegerStatement(value)=>value.toString
      case DoubleStatement(value)=>value.toString
      case TrueStatement()=>"true"
      case FalseStatement()=>"false"
      case IdentifierStatement(value)=>value
      case FunctionCallStatement(funcid, args)=>getCall(funcid, args)
      case NoArgs()=>""
    }
  }
  */
  
  def functioncalltranslator(call:aFunctionCallStatement, callingthread:String):String =
  {
    //println("in functioncalltranslator. call is "+call)
    //if(call.functionidentifier=="plus"){println("found")}
    
    call match
    {
      case aFunctionCallStatement("actionPrint",_, Left(StringStatement(value))) => "print" + callingthread + ".push_back(" + value + ")"
      case aFunctionCallStatement("actionPrint",_, Left(IdentifierStatement(value))) => "print" + callingthread + ".push_back(std::to_string(" + value + "))"
      case aFunctionCallStatement("actionPrint",_, Left(x:aFunctionCallStatement)) => "print" + callingthread + ".push_back(" + functioncalltranslator(x,callingthread) + ")"
      case aFunctionCallStatement("toString",_, Left(x:aFunctionCallStatement)) => "std::to_string(" + functioncalltranslator(x,callingthread) + ")"
      case aFunctionCallStatement("toString",_, Left(IdentifierStatement(value))) => "std::to_string(" + value + ")"
      case aFunctionCallStatement("actionMutate",_, Right(aNamedCallargs(List(aNamedCallarg(IdT("newValue"),IdentifierStatement(newval)), aNamedCallarg(IdT("variable"),IdentifierStatement(vari)))))) => vari + " = " + newval
      case aFunctionCallStatement("actionMutate",_, Right(aNamedCallargs(List(aNamedCallarg(IdT("newValue"),x:aFunctionCallStatement), aNamedCallarg(IdT("variable"),IdentifierStatement(vari)))))) =>
      {
        "write" + vari.capitalize + " = " + functioncalltranslator(x, callingthread)
      }
      case aFunctionCallStatement("plus",_,_) => basicmathcalltranslator(call, callingthread)
      case aFunctionCallStatement("minus",_,_) => basicmathcalltranslator(call, callingthread)
      case aFunctionCallStatement("multiply",_,_) => basicmathcalltranslator(call, callingthread)
      case aFunctionCallStatement("divide",_,_) => basicmathcalltranslator(call, callingthread)
      case aFunctionCallStatement(funcid,cppfuncid,args) =>
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
      case aFunctionCallStatement(_,_, Right(aNamedCallargs(callargs))) =>
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
  
  /*
  def functioncalltranslator(call:aFunctionCallStatement, callingthread:String, getCall:((String,Either[Callarg,NamedCallargs])=>String)):String =
  {
    //println("in functioncalltranslator. call is "+call)
    //if(call.functionidentifier=="plus"){println("found")}
    
    call match
    {
      case aFunctionCallStatement("actionPrint",_, Left(StringStatement(value))) => "print" + callingthread + ".push_back(" + value + ")"
      case aFunctionCallStatement("actionPrint",_, Left(IdentifierStatement(value))) => "print" + callingthread + ".push_back(std::to_string(" + value + "))"
      case aFunctionCallStatement("actionPrint",_, Left(x:FunctionCallStatement)) => "print" + callingthread + ".push_back(" + functioncalltranslator(x,callingthread,getCall) + ")"
      case aFunctionCallStatement("toString",_, Left(x:FunctionCallStatement)) => "std::to_string(" + functioncalltranslator(x,callingthread,getCall) + ")"
      case aFunctionCallStatement("toString",_, Left(IdentifierStatement(value))) => "std::to_string(" + value + ")"
      case aFunctionCallStatement("actionMutate",_, Right(NamedCallargs(List(NamedCallarg(IdT("newValue"),IdentifierStatement(newval)), NamedCallarg(IdT("variable"),IdentifierStatement(vari)))))) => vari + " = " + newval
      case aFunctionCallStatement("actionMutate",_, Right(NamedCallargs(List(NamedCallarg(IdT("newValue"),x:FunctionCallStatement), NamedCallarg(IdT("variable"),IdentifierStatement(vari)))))) =>
      {
        "write" + vari.capitalize + " = " + functioncalltranslator(x, callingthread,getCall)
      }
      case FunctionCallStatement("plus",_,_) => basicmathcalltranslator(call, callingthread, getCall:((String,Either[Callarg,NamedCallargs])=>String))
      case FunctionCallStatement("minus",_,_) => basicmathcalltranslator(call, callingthread, getCall:((String,Either[Callarg,NamedCallargs])=>String))
      case FunctionCallStatement("multiply",_,_) => basicmathcalltranslator(call, callingthread, getCall:((String,Either[Callarg,NamedCallargs])=>String))
      case FunctionCallStatement("divide",_,_) => basicmathcalltranslator(call, callingthread, getCall:((String,Either[Callarg,NamedCallargs])=>String))
      case FunctionCallStatement(funcid,args) => getCall(funcid,args)
      case _=> "not implemented"
    }
  }
  
  def basicmathcalltranslator(call:aFunctionCallStatement, callingthread:String, getCall:((String,Either[Callarg,NamedCallargs])=>String)):String=
  {
    val operator = if(call.functionidentifier=="plus"){" + "}else if(call.functionidentifier=="minus"){" - "}else if(call.functionidentifier=="multiply"){" * "}else if(call.functionidentifier=="minus"){" / "}
    call match
    {
      //case FunctionCallStatement(_, Right(NamedCallargs(List(NamedCallarg(IdT("left"),IdentifierStatement(left)), NamedCallarg(IdT("right"),IdentifierStatement(right)))))) => left + operator + right
      //case FunctionCallStatement(_, Right(NamedCallargs(List(NamedCallarg(IdT("left"),IdentifierStatement(left)), NamedCallarg(IdT("right"),IntegerStatement(right)))))) => left + operator + right
      //case FunctionCallStatement(_, Right(NamedCallargs(List(NamedCallarg(IdT("left"),IdentifierStatement(left)), NamedCallarg(IdT("right"),DoubleStatement(right)))))) => left + operator + right
      case aFunctionCallStatement(_,_, Right(NamedCallargs(callargs))) =>
      {
        val argstr = callargs.map(arg=>
          {
            arg match
            {
              case aNamedCallarg(_,IdentifierStatement(value))=>value
              case aNamedCallarg(_,IntegerStatement(value))=>value.toString
              case aNamedCallarg(_,DoubleStatement(value))=>value.toString
              case aNamedCallarg(_,f:FunctionCallStatement)=>functioncalltranslator(f, callingthread, getCall)
            }
          }
        ):List[String]
        "(" + argstr(0) + operator + argstr(1) + ")"
      } 
    }
  }
  */
  
  
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
case class aDefLhs(val description:DefDescriptionT, val id:IdT, val cppid:IdT, val args:Option[Arguments], val returntype:TypeT)
//case class Arguments(val args:List[Argument])
//case class Argument(val id:IdT, val typestr:TypeT)
case class aDefRhs(val expressions:List[aExpression] )
case class aNamedCallarg(id:IdT, argument:aCallarg) //extends Callarg
case class aNamedCallargs(val value:List[aNamedCallarg])

case class aFunctionCallStatement(val functionidentifier:String, val cppfunctionidentifier:String, val args:Either[aCallarg,aNamedCallargs]) extends aStatement with aCallarg










