package fumurtCompiler


object FumurtTypeChecker
{
  def check(in:List[Definition]):Option[List[FumurtError]] =
  {
    val providedTypes = List("Integer", "Double", "Boolean", "String", "Nothing")
    val print = DefLhs(ActionT(), IdT("actionPrintln"), Some(Arguments(List(Argument(IdT("toPrint"), TypeT("String"))))), TypeT("Nothing"))
    //val multiply = DefLhs(FunctionT, IdT("multiply"), Some(Arguments(List(Argument(IdT("left"),TypeT("Integer")), Argument(IdT("Right"), TypeT("Integer"))))), )
    val sum = DefLhs
    val divide = DefLhs
    val subtract = DefLhs
    val fumurtif = DefLhs
    val basics = List(multiply, plus, divide, minus, mutate, print)
    
    
    //all standard library functions available everywhere (maybe also actions). 
    //checkexpression(in, DefLhs(UnsafeActionT(), IdT(""), None, TypeT("Nothing")), None, List(List():List[Definition]), basics, List():List[DefLhs], List():List[FumurtErrors])
    
    val errors = checktop(in, basicfunctions)
    
    if (errors.isEmpty)
    {
      None
    }
    else
    {
      Some(errors)
    }
  }
  
  def checktop(in:List[Definition], basicFunctions:List[DefLhs]): List[FumurtError]=
  {
    val topdefs = indexlefts(in)
    val program = topdefs.filter(x=>(x.leftside.description match {case ProgramT => true; case _=> false})
    val implicitargs = indexlefts(topdefs.filter(x=>(x.leftside.description match {case ProgramT => false; case _=> true})))
    checkprogram(program, implicitargs, basicFunctions) ++ checkexpressions(in.filter(x=>(x.leftside.description match {case ProgramT => false; case _=> true})), None, Some(implicitargs), basicFunctions, 0) 
  }
  
  def checkprogram(program:Definition, topleveldefs:List[DefLhs], basicFunctions:List[DefLhs]): List[FumurtError]=
  {
    def checkuseofthread(program, deflhs):List[FumurtError]=
    {
      deflhs.description match
      {
        case ThreadT() => program.rightside.find(y=>y match{case FunctionCallStatement(x.id.value, _) => true; case _=>false}) 
          match
          { 
            case Some(_)=> List(); 
            case None=> List(FumurtError(x.pos, "thread "+x.id.value+" is declared but not used"))
          }
        case _=> None
      }
    }
    val unusedthreaderrors:List[FumurtError] = topleveldefs.foldLeft((x,y)=>checkuseofthread(program,x)++checkuseofthread(program,y))
    
    val unsuitabledefinitions = ListBuffer()
    for (i<-program.rightside.expressions)
    {
      i match
      {
        case x:Definition => 
        {
          x.leftside.description match
          {
            case SynchronizedVariableT() => {}
            case _=> {unsuitabledefinitions :+ List(FumurtError(x.pos,"Do not define functions, actions or unsynchronized values in Program"))}
          }
        }
        case _=>{}
      }
    }
    
    unusedthreaderrors ++ unsuitabledefinitions.toList
  }
  
  def checkexpressions(tree:List[Expression], containingdefinition:Option[Definition], arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs]):List[FumurtError]=
  {
    tree.foldLeft(List())((x,y)=>checkexpression(x, depth)++checkexpression(y))
  }
  
  def checkexpression(tocheck:Expression, containingdefinition:Option[Definition], arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs]):List[FumurtError] =
  {
    tocheck match
    {
      case x:Definition=>
      { 
        val localscope = containingdefinition match indexlefts(containingdefinition)
        {
          case None => List()
          case Some(contdef) => indexlefts(containingdefinition.rightside.expressions)
        }
        val (newargs, argpropagationerrors) = x.leftside.args match
        {
          case None => (List(), List())
          case Some(Arguments(args)) => 
          {
            val hits = args.flatmap(arg=>(arguments++inSameDefinition).find(y=>y.id.value==arg.id.value))
            if (hits.length !=args.length)
            {
              (hits, List())
            }
            else {(hits, List(FumurtError(x.pos,"One or more arguments not found in local scope")))}
          }
        }
        checkdefinition(x, x.leftside, newargs, basicFunctions) ++ argpropagationerrors
      }
      case x:Statement => checkstatement(x, containingdefinition, arguments, basicFunctions, inSameDefinition, currentErrors)
    }
  }
  
  def checkstatement(tocheck:Statement, containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs]): List[FumurtError]=
  {
    tocheck match
    {
      case b:BasicValueStatement=> checkbasicvaluestatement(containingdefinition.returntype, b, "Return")
      case b:IdentifierStatement=>
      {
        val statedvalue = findinscope(same name as b)
        statedvalue match
        {
          case Left(string) => List(FumurtError(b.pos, string))
          case Right(deflhs) => 
          {
            if(containingdefinition.returntype.value != deflhs.returntype.value)
            {
              List(FumurtError(b.pos, "expected: " +containingdefinition.returntype.value+ ". Got: " +deflhs.returntype.value))
            }
            else
            {
              List()
            }
          }
        }
      }
      case y:FunctionCallStatement("if", args)=>
      {
        checkifcall(y, containingdefinition.returnType)
      }
      case y:FunctionCallStatement=>
      {
        val calledfunction = findinscope(same name as y)
        val argumenterrors = y.args match 
        {
          case Left(NoArgs) => calledfunction.args match
          {
            case None => List()
            case Some(_) => List(FumurtError(y.pos, "expected arguments, but none were given")) 
          }
          case Left(callarg) => checkCallarg(calledfunction.)
          case Right(NamedCallargs(value)) => 
          {
            
          }
        }
        val returnerror = if (containingdefinition.returntype != calledfunction.returntype)
        {
          List(FumurtError(y.pos, "Expected return type: "+containingdefinition.returntype+". Got: "+calledfunction.returntype))
        }
        else {List()}
        returnerror ++ argumenterrors
      }
    }
  }
  
  def checkifcall(ifcall:FunctionCallstatement, expectedtype:TypeT):List[FumurtError] =
  {
    ifcall.args match
    {
      case Left(Callarg) => List(FumurtError(ifcall.pos, "Call to if needs three arguments"))
      case Right(NamedCallArgs(arglist))=>
      {
        if (arglist.length != 3)
        {
          List(FumurtError(ifcall.pos, "Call to if needs three arguments"))
        }
        else
        {
          if(arglist(0).id != "condition"){List(FumurtError(ifcall.pos, "Call to if needs a condition argument"))} else {List()}
          ++
          if(arglist(1).id != "else"){List(FumurtError(ifcall.pos, "Call to if needs an else argument"))} else {List()}
          ++
          if(arglist(2).id != "then"){List(FumurtError(ifcall.pos, "Call to if needs a then argument"))} else {List()}
          ++
          arglist.foldLeft(List())((x,y)=>checkCallarg(x)++checkCallarg(y))
        }
      }
    }
  }
  
  def checknamedcallargs(calledfunction:DefLhs, namedcallargs:List[NamedCallarg], containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs], currentErrors:List[FumurtError]):List[FumurtError] =
  {
    calledfunction.args match
    {
      case None => List(FumurtError("No arguments expected, but "+namedcallargs.length+" were given"))
      case Some(defargs) => 
      {
        if (defargs.length != value.length) 
        {
          List(FumurtError(y.pos, "expected "+defargs.length+" arguments. Got "+value.length+" arguments"))
        } 
        else 
        {
          if(!value.groupBy(x=>x.id.value).filter((x,y)=>y.length>1).isEmpty) //ensure uniqueness of arguments
          {
            List(FumurtError(y.pos, "two or more arguments were given with the same name"))
          }
          else 
          {
            val individualargumenterrors = ListBuffer()
            for(i<-0 until value.length)
            {
              individualargumenterrors ++ if(value(i).id.value != defargs(i).id.value) 
              {
                List(FumurtError(y.pos, "Wrong argument name. Argument in definition named "+defargs(i).id.value+". In calling named "+value(i).id.value ))
              } 
              else 
              {
                checkCallarg(defargs(i).typestr, value(i).argument, containingdefinition, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs], currentErrors:List[FumurtError])
              }
            }
            individualargumenterrors.toList
          }
        }
      }
    }
  }
  
  def checkCallarg(expectedtype:TypeT, arg:Callarg, containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs], currentErrors:List[FumurtError]):List(FumurtError) = 
  {
    arg match
    {
      case c:BasicValueStatement=> checkbasicvaluestatement(containingdefinition.returntype, c, "Call argument")
      case c:NoArgs => calledfunction.args match
      {
        println("NoArgs got checked by checkCallarg. This is better checked in checkstatement"); scala.sys.exit()
      }
      case c:IdentifierStatement => 
      {
        val thingdef = findinscope(same name as c)
        if(expectedtype.value != thingdef.returntype.value)
        {
          List(FumurtError(c.pos, "Expected type "+expectedtype.value+". Got "+thingdef.returntype.value))
        }
        else {List()}
      }
      case c:FunctionCallStatement => 
      {
        val functioncallarg = findinscope(same name as c)
        //check that call end result is correct
        val resulterrors = if(expectedtype.value != functioncallarg.returntype.value)
        {
          List(FumurtError(c.pos, "Expected function to return argument of type "+expectedtype.value+". Got "+functioncallarg.returntype.value))
        }
        else {List()}
        //check that call itself is correct
        val callerrors = checkstatement(c, containingdefinition, arguments, basicFunctions, inSameDefinition, currentErrors)
        callerrors ++ resulterrors
      }
    }
  }
  
  def checkbasicvaluestatement(expectedtype:TypeT, bascistatement:BasicStatement, role:String):List(FumurtError) =
  {
    basicstatement match
    {
      case c:StringStatement => {if (expectedtype.value != "String") List(FumurtError(c.pos, role+" type should be "+expectedtype.value+"\n"+role+" type was String")) else List()}
      case c:IntegerStatement => {if (expectedtype.value != "Integer") List(FumurtError(c.pos, role+" type should be "+expectedtype.value+"\n"+role+" type was Integer")) else List()}
      case c:DoubleStatement => {if (expectedtype.value != "Double") List(FumurtError(c.pos, role+" type should be "+expectedtype.value+"\n"+role+" type was Double")) else List()}
      case c:TrueStatement => {if (expectedtype.value != "Boolean") List(FumurtError(c.pos, role+" type should be "+expectedtype.value+"\n"+role+" type was Boolean")) else List()}
      case c:FalseStatement => {if (expectedtype.value != "Boolean") List(FumurtError(c.pos, role+" type should be "+expectedtype.value+"\n"+role+" type was Boolean")) else List()}
    }
  }
  
  def checkdefinition(tocheck:Definition, containingdefinition:Option[DefLhs], arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs]): List[FumurtError]=
  {
    val undererrors = checkexpressions(tocheck.rightside.expressions, tocheck, )
    val nameerror = tocheck.leftside.description match
    {
      case ActionT() => if(!tocheck.leftside.id.value.beginsWith("action")){List(FumurtError(tocheck.pos, "Name of action is not prefixed with \"action\""))} else{List()}
      case ThreadT() => if(!tocheck.leftside.id.value.beginsWith("thread")){List(FumurtError(tocheck.pos, "Name of thread is not prefixed with \"thread\""))} else{List()}
      case FunctionT() => List()
      case ValueY() => List()
      case ProgramT() => println("Program got checked by checkdefinition. This is better checked in checkprogram"); scala.sys.exit()
    }
    val peermissionerror = tocheck.leftside.description match
    {
      case ActionT() => containingdefinition match{ case } List(FumurtError(tocheck.pos, "actions cannot be defined in values or functions"))
      case ThreadT() => containingdefinition match{ case None => List(); case Some(_)=>List(FumurtError(tocheck.pos, "threads must be defined on top"))}
      case FunctionT() => containingdefinition match{ case Some(ValueT) => List(FumurtError(tocheck.pos, "functions cannot be defined in values")); case _=> List()}
      case SynchronizedVariableT() => List(FumurtError(tocheck.pos, "synchronized variables must be defined in Program definition"))
      case ValueT() => List()
      case ProgramT() => println("Program got checked by checkdefinition. This is better checked in checkprogram"); scala.sys.exit()
    }
    
    undererrors.toList ++ nameerror ++ permissionerror
  }
  
  
  def indexlefts(in:List[Expression]):List[DefLhs]=
  {
    in.head match
    {
      case Definition(leftside, _)=>
      {
        leftside +: indexlefts(in.tail)
      }
      case _:Statement=>
      {
        indexlefts(in.tail)
      }
    }
  }
  
  def findinscope(arguments:Option[List[DefLhs]], inscope:List[DefLhs], basicfunctions:List[DefLhs], searchFor:String):Either[String, DefLhs]=
  {
    val argsres = arguments match{ case Some(args)=>args.args.filter(x=>x.id.value==searschFor); case None=>List():List[DefLhs]}
    val inscoperes = inscope.filter(x=>x.id.value==searchFor)
    val basicfunctionres = basicfunctions.filter(x=>x.id.value==searchFor)
    val res = argsres ++ inscoperes ++ basicfunctionres
    
    if(res.length == 1)
    {
      Right(res.head)
    }
    else if(res.length>1)
    {
      Left("Ambiguous reference to "+searchFor)
    }
    else if(res.length == 0)
    {
      Left(searchFor+" not found")
    }
    else
    {
      Left("error in search for "+searchFor)
    }
  }
  
  /*def indexargumentlefts(argumentspassed:Option[List[Arguments]], argumentleftsgiven:Option[List[DefLhs]], leftsincallingdefinition:List[DefLhs], basicfunctions:List[DefLhs])
  {
    
  }*/
}

class DefinitionC(val location:List[String], val outType:String, val inTypes:Option[List[ArgumentC]], typee:DefinitionType)
case class ArgumentC(name:String, typee:String) 

class DefinitionType()

case class FunctionType() extends DefinitionType
case class ActionType() extends DefinitionType
case class UnsafeActionType() extends DefinitionType
case class ValueType() extends DefinitionType







