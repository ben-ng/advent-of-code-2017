val input = 1234

// Part 1
UlamSpiralIterator(startAtCounter = input)
  .next match { case (pos: Coord, _) =>
    math.abs(pos.x) + math.abs(pos.y)
  }

// The companion object lets you start the iterator at any counter
// or coordinate in constant time.
// Notes on deriving the closed form are in the footer.
object UlamSpiralIterator {
  def apply(startAtCounter: Int) = {
    val closestYIndex = -(math.floor(1d/8 * (5 + math.sqrt(-7 + 16 * startAtCounter))).toInt - 1)
    new UlamSpiralIterator(startAtY = closestYIndex)
      .dropWhile(_._2 < startAtCounter)
  }

  def apply(startAtCoord: Coord) = {
    new UlamSpiralIterator(startAtY = math.min(startAtCoord.y + 1, 0))
      .dropWhile(_._1 != startAtCoord)
  }
}

class UlamSpiralIterator(startAtY: Int = 0)
  extends Iterator[(Coord, Int)] {

  assert(startAtY <= 0, "Only negative y indices are supported")

  var count = {
    val gen = (-startAtY) + 1
    2 - 5 * gen + 4 * gen * gen
  }
  var pos = Coord(0, startAtY)
  var currentBounds = {
    val w = math.abs(startAtY)
    Bounds(-w, w, -w, w)
  }
  val deltas = Iterator.continually(List(
    Coord(1, 0),  // Right
    Coord(0, 1),  // Up
    Coord(-1, 0), // Left
    Coord(0, -1)  // Down
  )).flatten
  var currentDelta = deltas.next()

  def hasNext = true

  def next() = {
    val oldCount = count
    val oldPos = pos
    val oldBounds = currentBounds

    pos += currentDelta
    count += 1
    currentBounds = currentBounds containing pos

    if (oldBounds != currentBounds) currentDelta = deltas.next()

    oldPos -> oldCount
  }
}

case class Coord(x: Int, y: Int) {
  def +(b: Coord) = Coord(x + b.x, y + b.y)
}
case class Bounds(xMin: Int, xMax: Int, yMin: Int, yMax: Int) {
  def containing(a: Coord) = Bounds(
    math.min(xMin, a.x),
    math.max(xMax, a.x),
    math.min(yMin, a.y),
    math.max(yMax, a.y))
}

// Test cases for UlamSpiralIterator
List(
  (Coord(0,0), 1),
  (Coord(1,0), 2),
  (Coord(-1,0), 6),
  (Coord(-1,-1), 7),
  (Coord(0,-1), 8),
  (Coord(1,-1), 9),
  (Coord(2,-2), 25)
) foreach { case (coord, counter) =>
  assert(UlamSpiralIterator(startAtCounter = counter).next() ==
    UlamSpiralIterator(startAtCoord = coord).next())
}

// To find the closed form, first generate the numbers on the vertical axis
// (new UlamSpiralIterator)
//   .takeWhile({case (coord, _) => coord.y > -10})
//   .filter({case (coord, _) => coord.x == 0 && coord.y <= 0})
//   .map({case (_, counter) => counter})
//   .mkString(" ")
//
// Then, use the fundamental theorem of algebra:
// http://www.wolframalpha.com/input/?i=interpolate+1+8+23+46+77+116+163+218
// Which reveals the closed form: 2 - 5 x + 4 x^2
//
// We can solve for x too:
// http://www.wolframalpha.com/input/?i=solve+2+-+5+x+%2B+4+x%5E2+%3D+y
// And keep the equation with positive solutions: 1/8 (5 + sqrt(-7 + 16 y))
