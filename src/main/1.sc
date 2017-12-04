val input = "1234"

val repeating = Iterator.continually(input.toCharArray)
                .flatMap(_.toIterable)
                .map(_.asDigit)

val thrice = repeating.take(input.length * 3).toList
val thriceNext = repeating.slice(1, input.length * 3 + 1).toList

{
  for ((cur, next) <- thrice zip thriceNext)
    yield if (cur == next) cur else 0
}.slice(input.length, input.length * 2).sum
