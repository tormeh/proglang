package fumurtCompiler


object FumurtTypeChecker
{
  def check(in:List[Definition]):Option[List[FumurtError]] =
  {
    val providedTypes = List("Integer", "Double", "Boolean", "String", "Nothing")
    val println = DefLhs(ActionT(), IdT("actionPrintln"), Some(Arguments(List(Argument(IdT("toPrint"), TypeT("String"))))), TypeT("Nothing"))
    val basicsides = List(println)
    //val basics = List(multiply, sum, divide, subtract)
    
    
    //step 1: make list of object/function/action definitions and their scope/location
    //step 2: check that all statements and definitions uses definitions that are in scope. Also that actions are not called from functions.
    //step 3: check that all definitions have a left part that matches their right part
    //step 4: check that all returning statements have type corresponding to the definition they belong to
    
    
    //all standard library functions available everywhere (maybe also actions). 
    //checkexpression(in, DefLhs(UnsafeActionT(), IdT(""), None, TypeT("Nothing")), None, List(List():List[Definition]), basics, List():List[DefLhs], List():List[FumurtErrors])
    
    None
  }
  
  
  
  def checkexpression(tree:List[Expression], leftside:DefLhs, arguments:Option[List[DefLhs]] libs:List[List[Definition]], basicFunctions:List[DefLhs], inScope:List[DefLhs], currentErrors:List[FumurtError]):Option[List[FumurtError]]=
  {
    if (!tree.isEmpty)
    {
      tree.head match
      {
        case Definition(leftside, rightside)=>
        {
          val localscope = indexlefts(rightside.expressions)
        }
        case x:Statement=>
        {
          //if return != Nothing, check that it is equal to the tree.head's return type
          x match
          {
            case b:BasicValueStatement=>
            {
              b match
              {
                case c:StringStatement => {if (leftside.returntype.value != "String") Some(List(FumurtError(c.pos, "Return type should be "+leftside.returntype.value+"\nReturn type was String"))) else None}
                case c:IntegerStatement => {if (leftside.returntype.value != "Integer") Some(List(FumurtError(c.pos, "Return type should be "+leftside.returntype.value+"\nReturn type was Integer"))) else None}
                case c:DoubleStatement => {if (leftside.returntype.value != "Double") Some(List(FumurtError(c.pos, "Return type should be "+leftside.returntype.value+"\nReturn type was Double"))) else None}
                case c:TrueStatement => {if (leftside.returntype.value != "Boolean") Some(List(FumurtError(c.pos, "Return type should be "+leftside.returntype.value+"\nReturn type was Boolean"))) else None}
                case c:FalseStatement => {if (leftside.returntype.value != "Boolean") Some(List(FumurtError(c.pos, "Return type should be "+leftside.returntype.value+"\nReturn type was Boolean"))) else None}
              }
            }
            case b:IdentifierStatement=>
            {
              inScope.find(x => (x.id.value==b.value)) /*what about arguments and basic functions?*/ match
              {
                case Some(foundvalue) =>
                {
                  if(leftside.returntype.value != foundvalue.returntype.value)
                  {
                    Some(List(FumurtError(b.pos, "Return type should be "+leftside.returntype.value)))
                  }
                  else
                  {
                    None
                  }
                }
                case None => Some(List(FumurtError(b.pos, "Value out of scope or nonexistent")))
              }
            }
            case y:FunctionCallStatement=>
            {
              inscope.find()
            }
          }
        }
      }
    }
    else
    {
      None
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
  
  def findinscope(arguments:Option[List[DefLhs]], inscope:List[DefLhs], basicfunctions:List[DefLhs], searchFor:String):Either[TypeT, String]=
  {
    val argsres = arguments match{ case Some(args)=>args.args.filter(x=>x.id.value==searschFor); case None=>List():List[DefLhs]}
    val inscoperes = inscope.filter(x=>x.id.value==searchFor)
    val basicfunctionres = basicfunctions.filter(x=>x.id.value==searchFor)
    val res = argsres ++ inscoperes ++ basicfunctionres
    
    if(res.length == 1)
    {
      Right(TypeT(res.head))
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
}

class DefinitionC(val location:List[String], val outType:String, val inTypes:Option[List[ArgumentC]], typee:DefinitionType)
case class ArgumentC(name:String, typee:String) 

class DefinitionType()

case class FunctionType() extends DefinitionType
case class ActionType() extends DefinitionType
case class UnsafeActionType() extends DefinitionType
case class ValueType() extends DefinitionType








/*def makeDefinitionList(in:List[Expression], scopePath:List[String]):List[DefinitionC] =
  {
    in.head match
    {
      case Definition(leftside, rightside)=>
      {
        args = leftside.args match 
        {
          case None => None
          case Some(arguments) =>
          {
            
          }
        }
        
        
        DefinitionC(scopePath+leftside.id.value, leftside.returntype.value, args, leftside.) :+ makeDefinitionList(in.tail)
      }
      case Statement=>
      {
        makeDefinitionList(in.tail)
      }
    }
  
    //in.head :+ makeDefinitionList(in.tail)
  }*/
  
  /*def searchForDefinition(tree:List[Expression], askingDefinition:List[String], currentdepth:Int, searchFor:String):Option[Definition] =
  {
    if (!tree.isEmpty)
    {
      tree.head match
      {
        case Definition(left, right) =>
        {
          if (left.id.value == searchFor)
          {
            Some(left)
          }
          else
          {
            
          }
        }
        case Statement =>
        {
        
        }
      }
    }
    else
    {
      None
    }
    
    //searchForDefinition
    None
  }*/
