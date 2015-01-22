/*val easyr = """[a-z]*""".r
"lol" match {
  case easyr(_*) => println("matched!"); //println(x)
}
*/
val r = """("[^"]*")""".r
val er = """((?<="")(?:\\.|[^""\\])*(?=""))""".r

""""str"""" match {
  case r(x) => println("matched!"); println(x)
}

""""str"""" match {
  case er(x) => println("matched!"); println(x)
}
