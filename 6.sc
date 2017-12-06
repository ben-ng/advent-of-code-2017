import scala.collection.mutable

val input = wrapIntArray("0 2 7 0".split("\\s+").map(_.toInt))

val repeated = firstRepeatedState(input)

// Pt 1
repeated._1

// Pt 2
stepsInCycle(repeated._2)

def firstRepeatedState(input: mutable.WrappedArray[Int]
                      ): (Int, mutable.WrappedArray[Int]) = {
  val configurations = mutable.HashSet[mutable.WrappedArray[Int]]()

  redistribute(input, until = { input =>
    if (configurations contains input)
      true
    else {
      configurations += input.clone()
      false
    }
  })
}

def stepsInCycle(input: mutable.WrappedArray[Int]): Int = {
  val initialState = input.clone()
  redistribute(input, until = _ == initialState)._1
}

def redistribute(input: mutable.WrappedArray[Int],
                 until: (mutable.WrappedArray[Int] => Boolean)
                ): (Int, mutable.WrappedArray[Int]) = {

  def redistribute(step: Int): (Int, mutable.WrappedArray[Int]) = {
    val toRedistribute = input.max
    val idxToRedistribute = input.indexOf(toRedistribute)

    input.update(idxToRedistribute, 0)
    (for (idx <- (idxToRedistribute + 1) to (idxToRedistribute + toRedistribute))
      yield idx % input.length) foreach { idx: Int =>
      input.update(idx, input(idx) + 1)
    }

    if (until(input))
      (step + 1) -> input
    else {
      redistribute(step + 1)
    }
  }

  redistribute(0)
}
