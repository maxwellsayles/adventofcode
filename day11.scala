sealed abstract trait Part { def name: Char }
case class Generator(name: Char) extends Part
case class Chip(name: Char) extends Part

case class Floor(parts: List[Part]) {
  def generators: List[Generator] = 
    for (Generator(x) <- parts) yield Generator(x)

  def chips: List[Chip] = 
    for (Chip(x) <- parts) yield Chip(x)

  def isValid =
    if (generators.isEmpty) true
    else chips.forall(generators.contains)

  def isEmpty = parts.isEmpty

  // def moves = {
  //   val moveOneGenerator = removeOne(generators).map({
  //     case (x, xs) => (List(x), Floor(xs, chips))
  //   })
  //   val moveTwoGenerators = moveOneGenerator.flatMap({
  //     case (x, xs) => removeOne(xs).map({
  //       case (y, ys) => (y :: x, Floor(ys, chips))
  //     })
  //   }})
  //   val moveOneChip = removeOne(chips).map({case (x, xs) => (List(x), xs)})
  //   val moveTwoChips = moveOneChip.flatMap({case (x, xs) => {
  //     removeOne(xs).map({ case (y, ys) => (y :: x, ys)})
  //   }})
  //   val moveOneOfEach = for ((x, xs) <- moveOneGenerator)
  //     for ((y, ys) <- moveOneChip)
  //     yield (x ::: y, Floor(xs, ys))

  // }
}

// Represents the state of the puzzle, i.e. all floors and elevator location
case class State(  
  elevator: Int,
  floors: Array[Floor]
) {
  def isValid = floors.forall(_.isValid)

  def isFinished = floors.take(3).forall(_.isEmpty)

  // The normalized state is the set of pairsmatching chips and generators by
  // floor.
  def normalize = {
    val floorToGenerators =
      state.floors.zipWithIndex.flatMap({
        case (xs, i) => xs.generators.map(x => (x.name, i))
      }).toMap
    val floorToChips =
      state.floors.zipWithIndex.flatMap({
        case (xs, i) => xs.chips.map(x => (x.name, i))
      }).toMap
    val names = state.floors.flatMap(_.parts.map(_.name)).toSet
    names.toList.map(x => (floorToGenerators(x), floorToChips(x))).sorted
  }
}

// Represents the state of the solver.
case class Solver(
  stepCount: Int,
  inputs: List[State],
  outputs: List[State],
  visited: Set[State]
) {
  def step: Option[Int] = {
    (inputs, outputs) match {
      case (List(), List()) => None

      case (List(), _) =>
        Solver(stepCount + 1, outputs.reverse, List(), visited).step

      case (hd::tl, _) => {
        if (hd.isFinished) Some(stepCount)
        else if (!hd.isValid || visited.contains(hd))
          Solver(stepCount, tl, outputs, visited).step
        else {
          // WIP: Implement core logic
          Solver(stepCount, tl, outputs, visited + hd).step
        }
      }
    }
  }
}

def removeOne[T](xs: List[T]): List[(T, List[T])] =
  xs.zip(
    xs.inits.toList.reverse.init
      .zip(xs.tails.toList.tail)
      .map({case (x, y) => x ::: y})
  )

def initState: State = {
  val floor1 = Floor(List(Generator('s'), Generator('p'), Chip('s'), Chip('p')))
  val floor2 = Floor(List(Generator('t'), Generator('r'), Chip('r'), Generator('c'), Chip('c')))
  val floor3 = Floor(List(Chip('t')))
  val floor4 = Floor(List())
  State(0, Array(floor1, floor2, floor3, floor4))
}

println(initState.normalize)
