sealed abstract trait Part { def name: Char }
case class Generator(name: Char) extends Part
case class Chip(name: Char) extends Part

case class Floor(parts: List[Part]) {
  def generators: List[Generator] = 
    for (Generator(x) <- parts) yield Generator(x)

  def chips: List[Chip] = 
    for (Chip(x) <- parts) yield Chip(x)

  def isValid: Boolean =
    generators.isEmpty || chips.forall(generators.contains)

  def isEmpty: Boolean = parts.isEmpty

  def moveOne: List[(List[Part], Floor)] =
    removeOne(parts).map({
      case (x, xs) => (List(x), Floor(xs))
    }).filter(_._2.isValid)

  def moveTwo: List[(List[Part], Floor)] =
    moveOne.flatMap({
      case (List(x), Floor(xs)) =>
        removeOne(xs).map({
          case (y, ys) => (List(x, y), Floor(ys))
        })
    }).filter(_._2.isValid)
}

case class NormalizedState(
  elevator: Int,
  pairs: List[(Int, Int)]
)

// Represents the state of the puzzle, i.e. all floors and elevator location
case class State(  
  elevator: Int,
  floors: Array[Floor]
) {
  def isValid: Boolean = floors.forall(_.isValid)

  def isFinished: Boolean = floors.take(3).forall(_.isEmpty)

  // The normalized state is the set of pairsmatching chips and generators by
  // floor.
  def normalized: NormalizedState = {
    val floorToGenerators =
      floors.zipWithIndex.flatMap({
        case (xs, i) => xs.generators.map(x => (x.name, i))
      }).toMap
    val floorToChips =
      floors.zipWithIndex.flatMap({
        case (xs, i) => xs.chips.map(x => (x.name, i))
      }).toMap
    val names = floors.flatMap(_.parts.map(_.name)).toSet
    val pairs = names.toList.map(x => (floorToGenerators(x), floorToChips(x))).sorted
    NormalizedState(elevator, pairs)
  }
}

// Represents the state of the solver.
case class Solver(
  stepCount: Int,
  inputs: List[State],
  outputs: List[State],
  visited: Set[NormalizedState]
) {
  def step: Option[Int] = {
    (inputs, outputs) match {
      case (List(), List()) => None

      case (List(), _) =>
        Solver(stepCount + 1, outputs.reverse, List(), visited).step

      case (hd::tl, _) => {
        val normalized = hd.normalized
        if (hd.isFinished) Some(stepCount)
        else if (!hd.isValid || visited.contains(normalized))
          Solver(stepCount, tl, outputs, visited).step
        else {
          // WIP: Implement core logic
          Solver(stepCount, tl, outputs, visited + normalized).step
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

println(initState.normalized)
