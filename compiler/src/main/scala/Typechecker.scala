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
    checkexpression(in, DefLhs(UnsafeActionT(), IdT(""), None, TypeT("Nothing")), List(List():List[Definition]), basics, List():List[DefLhs])
    
    None
  }
  
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
<<<<<<< HEAD
=======
    None
  }*/
  
  def checkexpression(tree:List[Expression], leftside:DefLhs, libs:List[List[Definition]], basicFunctions:List[DefLhs], inScope:List[DefLhs]):Option[FumurtError]=
  {
    if (!tree.isEmpty)
    {
      tree.head match
      {
        case Definition=>
        {
        
        }
        case Statement=>
        {
          //if return != Nothing, check that it is equal to the tree.head's return type
          
        }
      }
    }
    else
    {
      None
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
