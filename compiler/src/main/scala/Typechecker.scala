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
    checkprogram(program, implicitargs, basicFunctions) ++ checkexpressions(in.filter(x=>(x.leftside.description match {case ProgramT => false; case _=> true}))) 
  }
  
  def checkprogram(program:Definition, topleveldefs:List[DefLhs], basicFunctions:List[DefLhs]): List[FumurtError]=
  {
    
  }
  
  def checkprogramcontents(exps:List[Expression]):Option[List[FumurtError]]=
  {
    
  }
  
  def checkexpressions(tree:List[Expression], containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError]):List[FumurtError]=
  {
    if (!tree.isEmpty)
    {
      tree.head match
      {
        case x:Definition=>
        {
          val localscope = indexlefts(x.rightside.expressions)
          currentErrors :+ checkdefinition(x, x.leftside, arguments, basicFunctions, inscope, currentErrors) 
        }
        case x:Statement => currentErrors :+ checkstatement(x, containingdefinition, arguments, basicFunctions, inScope, currentErrors)
      }
    }
    else
    {
      currentErrors
    }
  }
  
  def checkstatement(tocheck:Statement, containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs]): List[FumurtError]=
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
        checkifcall(y)
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
  
  def checknamedcallargs(calledfunction:DefLhs, namedcallargs:List[NamedCallarg], containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError]):List[FumurtError] =
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
                checkCallarg(defargs(i).typestr, value(i).argument, containingdefinition, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError])
              }
            }
            individualargumenterrors.toList
          }
        }
      }
    }
  }
  
  def checkCallarg(expectedtype:TypeT, arg:Callarg, containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError]):List(FumurtError) = 
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
        val callerrors = checkstatement(c, containingdefinition, arguments, basicFunctions, inScope, currentErrors)
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
  
  def checkdefinition(tocheck:Definition, containingdefinition:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError]): List[FumurtError]=
  {
    val errors = ListBuffer()
    for(expression<-tocheck.rightside.expressions)
    {
      errors = errors ++ expression match
      {
        case a:Definition =>
        {
          checkdefinition(a, containingdefinition, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs])
        }
        case a:Statement =>
        {
          checkstatement(a, tocheck, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs])
        }
      }
    }
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







