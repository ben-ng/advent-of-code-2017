val input = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"

val discTree = DiscNode(discs = input
                .split("\\n")
                .map(line => Disc(raw = line))
                .toList)

val pt1 = discTree.name

val imbalanced = discTree.imbalancedNode.get
val correctTotalWeight = discTree
                          .find(_.supports.map(_.name).contains(imbalanced.name))
                          .flatMap(_.supports.groupBy(_.totalWeight).find(_._2.length > 1))
                          .map(_._1)
                          .get
 val pt2 = correctTotalWeight - imbalanced.supports.map(_.totalWeight).sum

case class DiscNode(name: String, weight: Int, totalWeight: Int, supports: List[DiscNode]) {
  def imbalancedNode: Option[DiscNode] = {
    if (supports.isEmpty)
      Some(this)
    else {
      if (supports.map(_.totalWeight).distinct.length == 1)
        Some(this)
      else {
        supports
          .groupBy(_.totalWeight)
          .find(_._2.length == 1)
          .flatMap(_._2.headOption)
          .flatMap(_.imbalancedNode)
      }
    }
  }

  def find(cond: (DiscNode) => Boolean): Option[DiscNode] = {
    if (cond(this))
      Some(this)
    else
      supports.flatMap(_.find(cond)).headOption
  }
}
object DiscNode {
  private def findRootDisc(discs: List[Disc]): Disc = {
    def fixedPointIteration(rootDisc: Disc): Disc = {
      discs.find(_.supports.contains(rootDisc.name)) match {
        case Some(disc) => fixedPointIteration(rootDisc = disc)
        case None => rootDisc
      }
    }
    fixedPointIteration(rootDisc = discs.head)
  }

  def apply(discs: List[Disc]): DiscNode = {
    def treeFromDisc(root: Disc): DiscNode = {
      val supports = root.supports.map({ name =>
          treeFromDisc(discs.find(_.name == name).get)
      })

      DiscNode(name = root.name,
        weight = root.weight,
        totalWeight = root.weight + supports.map(_.totalWeight).sum,
        supports = supports)
    }
    treeFromDisc(findRootDisc(discs))
  }
}

// Really just for parsing the input
case class Disc(name: String, weight: Int, supports: List[String])
object Disc {
  val matcher = """^([a-z]+) \(([0-9]+)\)(?: -> )?([a-z, ]*)$""".r
  def apply(raw: String): Disc = {
    matcher.findFirstMatchIn(raw) match {
      case Some(matched) => {
        Disc(
          name = matched.group(1),
          weight = matched.group(2).toInt,
          supports = {
            if (matched.subgroups(2).isEmpty) Nil
            else matched.subgroups(2).split("[\\s,]+").toList
          }
        )
      }
      case None => throw new IllegalArgumentException
    }
  }
}
