val input = "0\n3\n0\n1\n-3"

// Pt1
escapeSteps(instructions = input.split("\\n").map(_.toInt),
  updateRule = _ + 1)

// Pt2
escapeSteps(instructions = input.split("\\n").map(_.toInt)
  , updateRule = i => if (i > 2) i - 1 else i + 1)

// Tried to do this with an immutable list but its so much slower :(
def escapeSteps(instructions: Array[Int], updateRule: (Int) => Int): Int = {
  def _escapeSteps(step: Int, index: Int): Int = {
    if (index >= instructions.length)
      step
    else {
      val newIndex = index + instructions(index)
      instructions.update(index, updateRule(instructions(index)))
      _escapeSteps(step = step + 1, index = newIndex)
    }
  }

  _escapeSteps(step = 0, index = 0)
}
