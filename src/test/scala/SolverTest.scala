package challenge

import org.scalatest._

class SolverTest extends FunSuite with Matchers {

  test(
    "solver should return right solution for map without crossings"
  ) {

    val value =
      """|@---A---+
         |        |
         |x-B-+   C
         |    |   |
         |    +---+"""

    val Right((visits, path)) = Solver(value)
    visits should be ("ACB")
    path should be ("@---A---+|C|+---+|+-B-x")
  }

  test(
    "solver should return right solution for map with crossings"
  ) {

    val value =  """|@         
                    || C----+  
                    |A |    |  
                    |+---B--+  
                    |  |      x
                    |  |      |
                    |  +---D--+"""

    val Right((visits, path)) = Solver(value)
    visits should be ("ABCD")
    path should be ("@|A+---B--+|+----C|-||+---D--+|x")
  }

  test(
    "solver should return right solution for map with crossings on chars different than - or |"
  ) {

    val value =  """|  @---+   
                    |      B   
                    |K-----|--A
                    ||     |  |
                    ||  +--E  |
                    ||  |     |
                    |+--E--Ex C
                    |   |     |
                    |   +--F--+"""

    val Right((visits, path)) = Solver(value)
    visits should be ("BEEFCAKE")
    path should be ("@---+B||E--+|E|+--F--+|C|||A--|-----K|||+--E--Ex")
  }

  test(
    "solver should return error for map without ending"
  ) {

    val value =
     """|  @---+   
        |      B   
        |K-----|--A
        ||     |  |
        ||  +--E  |
        ||  |     |
        |+--E--E- C
        |   |     |
        |   +--F--+"""

    val Left(error) = Solver(value)
    error should be ("No available tiles")
  }

  test(
    "solver should return error for map with multiple initial characters"
  ) {

    val value =
      """|  @---+   
         |      B   
         |K-----|--A
         ||     |  |
         ||  +--E  |
         ||  |     |
         |+--E--E- C
         |   |     |
         |@  +--F--+"""

    val Left(error) = Solver(value)
    error should be ("There is no unique initial position.")
  }

}