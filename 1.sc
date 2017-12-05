val input = "123123"

val repeating = Iterator.continually(input.toCharArray)
                .flatMap(_.toIterable)
                .map(_.asDigit)

def sumWithOffset(offset: Int): Int = {
  val prev = repeating.take(input.length).toList
  val next = repeating.slice(offset, input.length + offset).toList

  {
    for ((cur, next) <- prev zip next)
    yield if (cur == next) cur else 0
  }.sum
}

sumWithOffset(1)
sumWithOffset(input.length/2)
