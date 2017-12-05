import scala.collection.immutable.ListMap

firstLargerValue(347991)

def firstLargerValue(input: Int): Int = {
  new SpiralIterator(maxCounter = input)
    .find(_ > input)
    .getOrElse(-1)
}

class SpiralIterator(maxCounter: Int) extends Iterator[Int] {
  var memory = {
    val spiralRadius = math.floor(math.sqrt(maxCounter)).toInt
    // +1 fudge factor
    // * 8 to get the perimeter of the outer spiral
    // * 2 to include the inner spiral's perimeter
    val maxSize = (spiralRadius + 1) * 8 * 2
    val m = new LRUCache[Coord, Int](maxSize = maxSize)
    m += (Coord(0, 0), 1)
    m
  }
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
    count = {
      for (
        x <- pos.x - 1 to pos.x + 1;
        y <- pos.y - 1 to pos.y + 1
      ) yield Coord(x, y)
    }
    .filter(_ != pos)
    .map(memory.getOrElse(_, 0))
    .sum

    memory += (pos, count)

    currentBounds = currentBounds containing pos
    if (oldBounds != currentBounds) currentDelta = deltas.next()

    oldCount
  }
}

class LRUCache[K, V](maxSize: Int) {
  var map = ListMap[K, V]()

  def +=(kv: (K, V)): Unit = {
    map += kv

    if (map.size > maxSize)
      map = map.drop(1)

    ()
  }

  def getOrElse(key: K, default: V): V =
    map.getOrElse(key, default)
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