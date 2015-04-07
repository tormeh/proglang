package fumurtCompiler
import scala.collection.mutable.ListBuffer

object FumurtTypeChecker
{
  def check(in:List[Definition]):Option[List[FumurtError]] =
  {
    val providedTypes = List("Integer", "Double", "Boolean", "String", "Nothing")
    val print = DefLhs(ActionT(), IdT("actionPrintln"), Some(Arguments(List(Argument(IdT("toPrint"), TypeT("String"))))), TypeT("Nothing"))
    val multiply = DefLhs(FunctionT(), IdT("multiply"), Some(Arguments(List(Argument(IdT("toPrint"), TypeT("String"))))), TypeT("Nothing"))
    val plus = DefLhs(FunctionT(), IdT("plus"), Some(Arguments(List(Argument(IdT("left"), TypeT("Integer")), Argument(IdT("right"), TypeT("Integer"))))), TypeT("Integer"))
    val divide = DefLhs(FunctionT(), IdT("actionPrintln"), Some(Arguments(List(Argument(IdT("left"), TypeT("Integer")), Argument(IdT("right"), TypeT("Integer"))))), TypeT("Integer"))
    val minus = DefLhs(FunctionT(), IdT("minus"), Some(Arguments(List(Argument(IdT("left"), TypeT("Integer")), Argument(IdT("right"), TypeT("Integer"))))), TypeT("Integer"))
    val mutate = DefLhs(FunctionT(), IdT("actionMutate"), Some(Arguments(List(Argument(IdT("variable"), TypeT("Integer")), Argument(IdT("newValue"), TypeT("Integer"))))), TypeT("Nothing"))
    val basicfunctions = List(multiply, plus, divide, minus, mutate, print)
    
    
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
    val programs = in.filter(x=>(x.leftside.description match {case ProgramT() => true; case _=> false}))
    val implicitargs = topdefs.filter(x=>(x.description match {case ProgramT() => false; case _=> true}))
    val programerrors = if(programs.length==1)
    {
      checkprogram(programs(0), implicitargs, basicFunctions)
    }
    else {List(FumurtError(Global, "There must be exactly one program definition. "+programs.length+" program definitions detected"))}
    val othererrors = checkexpressions(in.filter(x=>(x.leftside.description match {case ProgramT() => false; case _=> true})), None, Some(implicitargs), basicFunctions) 
    
     programerrors++othererrors 
  }
  
  def checkprogram(program:Definition, topleveldefs:List[DefLhs], basicFunctions:List[DefLhs]): List[FumurtError]=
  {
    def checkuseofthread(program:Definition, thread:DefLhs):List[FumurtError]=
    {
      thread.description match
      {
        case ThreadT() => program.rightside.expressions.find(y=>y match{case FunctionCallStatement(thread.id.value, _) => true; case _=>false}) 
          match
          { 
            case Some(_)=> List(); 
            case None=> List(FumurtError(Global, "thread "+thread.id.value+" is declared but not used"))
          }
        case _=> List()
      }
    }
    val unusedthreaderrors:List[FumurtError] = topleveldefs.foldLeft(List():List[FumurtError])((x:List[FumurtError],y:DefLhs)=>x++checkuseofthread(program,y)):List[FumurtError]
    
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
  
  def checkexpressions(tree:List[Expression], containingdefinition:Option[Definition], containingdefinitionarguments:Option[List[DefLhs]], basicFunctions:List[DefLhs]):List[FumurtError]=
  {
    val insamedefinition = indexlefts(tree)
    tree.foldLeft(List():List[FumurtError])((x,y)=>x++checkexpression(y, containingdefinition, containingdefinitionarguments, basicFunctions, insamedefinition))
  }
  
  def checkexpression(tocheck:Expression, containingdefinition:Option[Definition], arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs]):List[FumurtError] =
  {
    tocheck match
    {
      case x:Definition=>
      { 
        val localscope = containingdefinition match
        {
          case None => List()
          case Some(contdef) => indexlefts(contdef.rightside.expressions)
        }
        val (newargs, argpropagationerrors) = x.leftside.args match
        {
          case None => (List(), List())
          case Some(Arguments(args)) => 
          {
            val hits = arguments match
            {
              case Some(contargs) => args.flatMap(arg=>(contargs++inSameDefinition).find(y=>y.id.value==arg.id.value))
              case None => args.flatMap(arg=>inSameDefinition.find(y=>y.id.value==arg.id.value))
            }
            if (hits.length !=args.length)
            {
              (hits, List())
            }
            else {(hits, List(FumurtError(x.pos,"One or more arguments not found in local scope")))}
          }
        }
        checkdefinition(x, Some(x.leftside), Some(newargs), basicFunctions) ++ argpropagationerrors
      }
      case x:Statement => containingdefinition match
      {
        case None => List(FumurtError(x.pos, "Statements must be enclosed in either Program or another definition"))
        case Some(contdef) => checkstatement(x, contdef.leftside, arguments, basicFunctions, inSameDefinition)
      }
    }
  }
  
  def checkstatement(tocheck:Statement, containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs]): List[FumurtError]=
  {
    tocheck match
    {
      case b:BasicValueStatement=> checkbasicvaluestatement(containingdefinition.returntype, b, "Return")
      case b:IdentifierStatement=>
      {
        val statedvalue = findinscope(arguments, inSameDefinition, basicFunctions, b.value)
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
      case y:FunctionCallStatement=>
      {
        if (y.functionidentifier=="if")
        {
          checkifcall(y, containingdefinition.returntype, containingdefinition, arguments, basicFunctions, inSameDefinition)
        }
        else
        {
          findinscope(arguments, inSameDefinition, basicFunctions, y.functionidentifier) match
          {
            case Left(string) => List(FumurtError(y.pos, "referenced function not found"))
            case Right(calledfunction) => 
            {
              val argumenterrors:List[FumurtError] = y.args match 
              {
                case Left(NoArgs()) => calledfunction.args match
                {
                  case None => List()
                  case Some(_) => List(FumurtError(y.pos, "expected arguments, but none were given")) 
                }
                case Left(callarg) => checkCallarg(calledfunction.returntype, callarg, containingdefinition, arguments, basicFunctions, inSameDefinition)
                case Right(NamedCallargs(value)) => 
                {
                  checknamedcallargs(calledfunction, value, containingdefinition, arguments, basicFunctions, inSameDefinition)
                }
              } 
              val returnerror:List[FumurtError] = if (containingdefinition.returntype != calledfunction.returntype)
              {
                List(FumurtError(y.pos, "Expected return type: "+containingdefinition.returntype+". Got: "+calledfunction.returntype))
              }
              else {List()}
              returnerror ++ argumenterrors
            }
          }
          
        }
      }
    }
  }
  
  def checkifcall(ifcall:FunctionCallStatement, expectedtype:TypeT, containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs]):List[FumurtError] =
  {
    ifcall.args match
    {
      case Left(callarg) => List(FumurtError(ifcall.pos, "Call to if needs three arguments"))
      case Right(NamedCallargs(arglist))=>
      {
        if (arglist.length != 3)
        {
          List(FumurtError(ifcall.pos, "Call to if needs three arguments"))
        }
        else
        {
          ( if(arglist(0).id != "condition"){List(FumurtError(ifcall.pos, "Call to if needs a condition argument"))} else {List()} )++
          ( if(arglist(1).id != "else"){List(FumurtError(ifcall.pos, "Call to if needs an else argument"))} else {List()} )++
          ( if(arglist(2).id != "then"){List(FumurtError(ifcall.pos, "Call to if needs a then argument"))} else {List()} )++
          checkCallarg(TypeT("Boolean"), arglist(0).argument, containingdefinition, arguments, basicFunctions, inSameDefinition)
        }
      }
    }
  }
  
  def checknamedcallargs(calledfunction:DefLhs, namedcallargs:List[NamedCallarg], containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs]):List[FumurtError] =
  {
    calledfunction.args match
    {
      case None => List(FumurtError(namedcallargs(0).id.pos, "No arguments expected, but "+namedcallargs.length+" were given"))
      case Some(Arguments(defargs)) => 
      {
        if (defargs.length != namedcallargs.length) 
        {
          List(FumurtError(namedcallargs(0).id.pos, "expected "+defargs.length+" arguments. Got "+namedcallargs.length+" arguments"))
        } 
        else 
        {
          if(!namedcallargs.groupBy(x=>x.id.value).filter(y=>y._2.length>1).isEmpty) //ensure uniqueness of arguments
          {
            List(FumurtError(namedcallargs(0).id.pos, "two or more arguments were given with the same name"))
          }
          else 
          {
            val individualargumenterrors = ListBuffer()
            for(i<-0 until namedcallargs.length)
            {
              individualargumenterrors ++ (if(namedcallargs(i).id.value != defargs(i).id.value) 
                {
                  List(FumurtError(namedcallargs(i).id.pos, "Wrong argument name. Argument in definition named "+defargs(i).id.value+". In calling named "+value(i).id.value ))
                }
                else 
                {
                  checkCallarg(defargs(i).typestr, value(i).argument, containingdefinition, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs], currentErrors:List[FumurtError])
                }
              )
            }
            individualargumenterrors.toList
          }
        }
      }
    }
  }
  
  def checkCallarg(expectedtype:TypeT, arg:Callarg, containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inSameDefinition:List[DefLhs]):List[FumurtError] = 
  {
    arg match
    {
      case c:BasicValueStatement=> checkbasicvaluestatement(containingdefinition.returntype, c, "Call argument")
      case c:NoArgs => 
      {
        println("NoArgs got checked by checkCallarg. This is better checked in checkstatement"); scala.sys.exit()
      }
      case c:IdentifierStatement => 
      {
        val thingdef = findinscope(arguments, inSameDefinition, basicFunctions, c.value)
        if(expectedtype.value != thingdef.returntype.value)
        {
          List(FumurtError(c.pos, "Expected type "+expectedtype.value+". Got "+thingdef.returntype.value))
        }
        else {List()}
      }
      case c:FunctionCallStatement => 
      {
        val functioncallarg = findinscope(arguments, inSameDefinition, basicFunctions, c.functionidentifier)
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
  
  def checkbasicvaluestatement(expectedtype:TypeT, bascistatement:BasicValueStatement, role:String):List[FumurtError] =
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
    val undererrors = checkexpressions(tocheck.rightside.expressions, tocheck )
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
      case ActionT() => containingdefinition match
      { 
        case None=>List()
        case Some(DefLhs(ValueT(),_))=> List(FumurtError(tocheck.pos, "actions cannot be defined in values"))
        case Some(DefLhs(FunctionT(),_))=> List(FumurtError(tocheck.pos, "actions cannot be defined in  functions"))
      }
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
  
  def findinscope(arguments:Option[List[DefLhs]], inSameDefinition:List[DefLhs], basicfunctions:List[DefLhs], searchFor:String):Either[String, DefLhs]=
  {
    val argsres = arguments match{ case Some(args)=>args.args.filter(x=>x.id.value==searschFor); case None=>List():List[DefLhs]}
    val inscoperes = inSameDefinition.filter(x=>x.id.value==searchFor)
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







