val input = "0: 3\n1: 2\n2: 5\n4: 4\n6: 6\n8: 4\n10: 8\n12: 8\n14: 6\n16: 8\n18: 6\n20: 6\n22: 8\n24: 12\n26: 12\n28: 8\n30: 12\n32: 12\n34: 8\n36: 10\n38: 9\n40: 12\n42: 10\n44: 12\n46: 14\n48: 14\n50: 12\n52: 14\n56: 12\n58: 12\n60: 14\n62: 14\n64: 12\n66: 14\n68: 14\n70: 14\n74: 24\n76: 14\n80: 18\n82: 14\n84: 14\n90: 14\n94: 17"

case class Layer(depth: Int, range: Int) {
  def period = (range - 1) * 2
  def severity = depth * range
}

case class Firewall(layers: List[Layer]) {
  def severity = layers.map(layer => {
                  if (layer.depth % layer.period == 0)
                    layer.severity
                  else
                    0
                }).sum

  def delayed(delay: Int) =
    Firewall(layers.map(l => Layer(l.depth + delay, l.range)))
}

val P = """([0-9]+): ([0-9]+)""".r
val firewall = Firewall(input
                        .split("\n")
                        .map {
                          case P(depth, range) =>
                            Layer(depth.toInt, range.toInt)
                        }
                        .toList)

val pt1 = firewall.severity

val pt2 = Stream
            .from(0)
            .find(delay => firewall.delayed(delay).severity == 0)
