val input = 1234

(new UlamSpiralIterator)
  .find({ case (pos: Coord, counter: Int) => counter == input })
  .map({ case (pos: Coord, counter: Int) =>
    math.abs(pos.x) + math.abs(pos.y)
  })
  .getOrElse(-1)

class UlamSpiralIterator extends Iterator[(Coord, Int)] {
  var count = 1
  var pos = Coord(0, 0)
  var currentBounds = Bounds(0, 0, 0, 0)
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
