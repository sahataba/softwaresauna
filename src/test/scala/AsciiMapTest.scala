package challenge

import org.scalatest.FunSuite

class AsciiMapTest extends FunSuite {

  test(
    "constructor should produce valid AsciiMap of size (2,2) for valid multiline string ascii map"
  ) {

    val str = """| @
                 |d """.stripMargin
    val Right(map) = AsciiMap(str)
    map.maxX === 2
    map.maxY === 2
  }
}