val input = "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"

val Parser = """([0-9]+) <-> ([0-9, ]+)""".r
val graph = input
              .split("\n")
              .map {
                case Parser(origin, destinations) =>
                  (origin.toInt, destinations.split(", ").map(_.toInt).toList)
              }
              .foldLeft(Map[Int, List[Int]]()) {
                case (accum, (origin, destinations)) =>
                  accum.updated(origin, destinations)
              }

def transitiveClosure(toVisit: List[Int],
                      visited: Set[Int] = Set()
                     ): List[Int] = {
  toVisit match {
    case Nil => visited.toList
    case head :: tl => {
      if (visited.contains(head))
        transitiveClosure(tl, visited)
      else
        transitiveClosure(tl ::: graph(head), visited + head)
    }
  }
}

val pt1 = transitiveClosure(toVisit = List(0)).length
val pt2 = graph
            .mapValues(transitiveClosure(_).sorted)
            .values
            .toList
            .distinct
            .length
