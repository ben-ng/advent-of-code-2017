val input = "5 1 9 5\n7 5 3\n2 4 6 8"

input.split("\n").map(row => {
  val rowDigits = row.split("\\s+").map(_.toInt)
  rowDigits.max - rowDigits.min
}).sum
