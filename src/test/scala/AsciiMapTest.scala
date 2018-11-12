package challenge

import org.scalatest._

class AsciiMapTest extends FunSuite with Matchers {

  test(
    "constructor should return AsciiMap of size (2,2) for valid multiline string ascii map"
  ) {

    val str = """| @
                 |d """.stripMargin

    val Right(map) = AsciiMap(str)
    map.maxX should be (2)
    map.maxY should be (2)
  }

  test(
    "constructor should return error for ascii map of different row sizes"
  ) {

    val str = """| @
                 |d tt""".stripMargin

    val Left(error) = AsciiMap(str)
    error should be ("java.lang.IllegalArgumentException: requirement failed: different row length")
  }

  test(
    "constructor should return error for empty ascii map"
  ) {

    val str = "".stripMargin

    val Left(error) = AsciiMap(str)
    error should be ("java.lang.IllegalArgumentException: requirement failed: x size equals 0")
  }
}