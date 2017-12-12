val input = "n,nw,nw,nw,sw,sw,sw,ne,s,s,nw,s,s,ne"

case class Coords(x: Int, y: Int , z: Int) {
  def +(o: Coords) = Coords(x + o.x, y + o.y, z + o.z)
  def distanceTo(o: Coords) = List(math.abs(x - o.x), math.abs(y - o.y), math.abs(z - o.z)).max
}

val path = input.split(",").map({
  case "n" => Coords(1, 0, -1)
  case "ne" => Coords(1, -1, 0)
  case "se" => Coords(0, -1, 1)
  case "s" => Coords(-1, 0, 1)
  case "sw" => Coords(-1, 1, 0)
  case "nw" => Coords(0, 1, -1)
}).foldLeft(List(Coords(0, 0, 0))) {
  case (accum: List[Coords], dir: Coords) =>
    accum ::: List(accum.last + dir)
}

val pt1 = path.last.distanceTo(Coords(0, 0, 0))
val pt2 = path.map(_.distanceTo(Coords(0, 0, 0))).max
