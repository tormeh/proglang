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
    val program = topdefs.filter(is program)
    val implicitargs = topdefs.filter(not program)
    checkprogram(program, implicitargs, basicFunctions) ++ checkexpressions(in.filter(not program)) 
  }
  
  def checkprogram(program:Definition, topleveldefs:List[Definition], basicFunctions:List[DefLhs]): List[FumurtError]=
  {
    
  }
  
  def checkprogramcontents(exps:List[Expression]):Option[List[FumurtError]]=
  {
    
  }
  
  def checkexpressions(tree:List[Expression], leftside:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError]):List[FumurtError]=
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
        case x:Statement => currentErrors :+ checkstatement(x, leftside, arguments, basicFunctions, inScope, currentErrors)
      }
    }
    else
    {
      currentErrors
    }
  }
  
  def checkstatement(tocheck:Statement, leftside:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError]): List[FumurtError]=
  {
    currentErrors ++ tocheck match
    {
      case b:BasicValueStatement=>
      {
        b match
        {
          case c:StringStatement => {if (leftside.returntype.value != "String") List(FumurtError(c.pos, "Return type should be "+leftside.returntype.value+"\nReturn type was String")) else List()}
          case c:IntegerStatement => {if (leftside.returntype.value != "Integer") List(FumurtError(c.pos, "Return type should be "+leftside.returntype.value+"\nReturn type was Integer")) else List()}
          case c:DoubleStatement => {if (leftside.returntype.value != "Double") List(FumurtError(c.pos, "Return type should be "+leftside.returntype.value+"\nReturn type was Double")) else List()}
          case c:TrueStatement => {if (leftside.returntype.value != "Boolean") List(FumurtError(c.pos, "Return type should be "+leftside.returntype.value+"\nReturn type was Boolean")) else List()}
          case c:FalseStatement => {if (leftside.returntype.value != "Boolean") List(FumurtError(c.pos, "Return type should be "+leftside.returntype.value+"\nReturn type was Boolean")) else List()}
        }
      }
      case b:IdentifierStatement=>
      {
        val statedvalue = findinscope(same name as b)
        statedvalue match
        {
          case Left(string) => List(FumurtError(b.pos, string))
          case Right(deflhs) => 
          {
            if(leftside.returntype.value != deflhs.returntype.value)
            {
              List(FumurtError(b.pos, "expected: " +leftside.returntype.value+ ". Got: " +deflhs.returntype.value))
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
            calledfunction.args match
            {
              case None => List(FumurtError("No arguments expected, but some were given"))
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
                    if (quantityerror.isEmpty)
                    {
                      for(i<-0 until value.length)
                      {
                        individualargumenterrors ++ if(value(i).id.value != defargs(i).id.value) 
                        {
                          List(FumurtError(y.pos, "Wrong argument name. Argument in definition named "+defargs(i).id.value+". In calling named "+value(i).id.value ))
                        } 
                        else 
                        {
                          checkCallarg(defargs(i).typestr, value(i).argument, leftside, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError])
                        }
                      }
                      individualargumenterrors.toList
                    }
                  }
                }
              }
            }
            
            
            
            
            
          }
          val returnerror = if (leftside.returntype != calledfunction.returntype)
          {
            List(FumurtError(y.pos, "Expected return type: "+leftside.returntype+". Got: "+calledfunction.returntype))
          }
          else {List()}
          returnerror ++ argumenterrors
        }
      }
    }
  }
  
  
  checkCallarg(expectedtype:TypeT, arg:Callarg, leftside:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError]):List(FumurtError) = 
  {
    arg match
    {
      case c:StringStatement => {if (expectedtype.value != "String") List(FumurtError(c.pos, "Argument type should be "+expectedtype.value+"\nArgument type was String")) else List()}
      case c:IntegerStatement => {if (expectedtype.value != "Integer") List(FumurtError(c.pos, "Argument type should be "+expectedtype.value+"\nArgument type was Integer")) else List()}
      case c:DoubleStatement => {if (expectedtype.value != "Double") List(FumurtError(c.pos, "Argument type should be "+expectedtype.value+"\nArgument type was Double")) else List()}
      case c:TrueStatement => {if (expectedtype.value != "Boolean") List(FumurtError(c.pos, "Argument type should be "+expectedtype.value+"\nArgument type was Boolean")) else List()}
      case c:FalseStatement => {if (expectedtype.value != "Boolean") List(FumurtError(c.pos, "Argument type should be "+expectedtype.value+"\nArgument type was Boolean")) else List()}
      case c:NoArgs => calledfunction.args match
      {
        println("NoArgs got checked by checkCallarg. This function is not supposed to be used that way"); scala.sys.exit()
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
    }
  }
  
  def checkdefinition(tocheck:Definition, leftside:DefLhs, arguments:Option[List[DefLhs]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError]): List[FumurtError]=
  {
  
  }
  
  def checkif()
  {
  
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
  
  def indexargumentlefts(argumentspassed:Option[List[Arguments]], argumentleftsgiven:Option[List[DefLhs]], leftsincallingdefinition:List[DefLhs], basicfunctions:List[DefLhs])
  {
    
  }
}

class DefinitionC(val location:List[String], val outType:String, val inTypes:Option[List[ArgumentC]], typee:DefinitionType)
case class ArgumentC(name:String, typee:String) 

class DefinitionType()

case class FunctionType() extends DefinitionType
case class ActionType() extends DefinitionType
case class UnsafeActionType() extends DefinitionType
case class ValueType() extends DefinitionType







