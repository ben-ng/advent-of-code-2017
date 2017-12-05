val part1input = "5 1 9 5\n7 5 3\n2 4 6 8"
val part2Input = "5 9 2 8\n9 4 7 3\n3 8 6 5"

part1input
  .split("\n")
  .map(row => {
    val rowDigits = row.split("\\s+").map(_.toInt)
    rowDigits.max - rowDigits.min
  })
  .sum

part2Input
  .split("\n")
  .flatMap(row => {
    val rowDigits = row.split("\\s+")
      .map(_.toInt)
      .sorted(Ordering[Int].reverse)

    for (
      bigger <- rowDigits;
      smaller <- rowDigits if smaller < bigger && bigger % smaller == 0
    ) yield bigger / smaller
  })
  .sum
