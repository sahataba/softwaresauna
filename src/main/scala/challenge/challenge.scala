package challenge

object Challenge extends App {

  val map1 =
    """|@---A---+
       |        |
       |x-B-+   C
       |    |   |
       |    +---+"""

  val map2 =
    """|@         
       || C----+  
       |A |    |  
       |+---B--+  
       |  |      x
       |  |      |
       |  +---D--+"""

  val map3 = 
   """|  @---+   
      |      B   
      |K-----|--A
      ||     |  |
      ||  +--E  |
      ||  |     |
      |+--E--Ex C
      |   |     |
      |   +--F--+"""

  val map4 =
    """|  @---+   
       |      B   
       |K-----|--A
       ||     |  |
       ||  +--E  |
       ||  |     |
       |+--E--E- C
       |   |     |
       |   +--F--+"""


  val correctSolutions = List(
    Right(("ACB",       "@---A---+|C|+---+|+-B-x")),
    Right(("ABCD",      "@|A+---B--+|+----C|-||+---D--+|x")),
    Right(("BEEFCAKE",  "@---+B||E--+|E|+--F--+|C|||A--|-----K|||+--E--Ex")),
    Left("No available tiles")
  )
  val solutions = List(map1, map2, map3, map4).map(Solver(_))
  println(solutions)
  require(solutions == correctSolutions)

}