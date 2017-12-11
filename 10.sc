val input = "46,41,212,83,1,255,157,65,139,52,39,254,2,86,0,204"

val pt1 = iterate(input.split(",").map(_.toInt).toList).lst.take(2) match {
  case a :: b :: Nil => a * b
  case _ => -1
}

val pt2 = hash(input)

case class IterState(lst: List[Int] = (0 to 255).toList, skip: Int = 0, pos: Int = 0)

def iterate(lengths: List[Int], lst: List[Int] = (0 to 255).toList, skip: Int = 0, pos: Int = 0): IterState = {
  lengths match {
    case Nil => IterState(lst = lst, skip = skip, pos = pos)
    case len :: tl => {
      iterate(lengths = tl,
      lst = Iterator
        .continually(lst)
        .flatten
        .slice(pos, pos + len)
        .toList
        .reverse
        .zipWithIndex
        .foldLeft(lst) { case (l, (v, i)) => l.updated((pos + i) % lst.length, v) },
        skip = skip + 1,
        pos = (pos + len + skip) % lst.length)
    }
  }
}

def hash(input: String) = {
  val lengths = input.toCharArray.map(_.toInt).toList ::: List(17, 31, 73, 47, 23)
  (0 until 64)
    .foldLeft(IterState()){
      case (IterState(lst, skip, pos), _) =>
        iterate(lengths = lengths, lst = lst, skip = skip, pos = pos)
    }
    .lst
    .grouped(16)
    .map(_.foldLeft(-1) {
      case (accum, next) => if (accum < 0) next else accum ^ next
    })
    .map(_.toHexString.toCharArray.reverse.padTo(2, "0").reverse.mkString(""))
    .mkString("")
}
