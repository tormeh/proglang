package de.tuberlin.uebb.comp1.homework

/**
  * Class representing a scope. For the moment it contains just a globalScope (defined everywhere) and a scope within a function.
  *	However, it can in the future be extended to namespaces, ...
  * */
abstract class Scope
/**
  *  A global scope
  * */
case object GlobalScope extends Scope{
  override def toString():String = "global"
}

/**
  * The name of the function (scope of the function)
  * @param name
  * */
case class ScopeFun(name:String) extends Scope{
  override def toString():String = "scope." + name
}
