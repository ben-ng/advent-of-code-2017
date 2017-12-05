val input = "aa bb aa\ndba dfe"

input.split("\n").count((row: String) => {
  val words = row.split("\\s+")
  words.distinct.length == words.length
})

input.split("\n").count((row: String) => {
  val words = row.split("\\s+").map(_.toCharArray.sorted.mkString)
  words.distinct.length == words.length
})
