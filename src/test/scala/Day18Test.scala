import org.scalatest.FunSuite

class Day18Test extends FunSuite {

  test("getValidPoints should work in the corners") {
    assert(Day18.getNeighbors((0,0)) == List( (1,0), (1,1), (0,1) ))
  }


}