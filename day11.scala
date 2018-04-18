// Represents a single floor in the puzzle
case class Floor(generators: List[Char], chips: List[Char]) {
  def isValid =
    if (generators.isEmpty) true
    else chips.forall(generators.contains)

  def isEmpty = generators.isEmpty && chips.isEmpty
}

// Represents the state of the puzzle, i.e. all floors and elevator location
case class State(  
  elevator: Int,
  floors: Array[Floor]
) {
  def isValid = floors.forall(_.isValid)

  def isFinished =
    floors(0).isEmpty && floors(1).isEmpty && floors(2).isEmpty
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

def initState = {
  val floor1 = Floor(List('s', 'p'), List('s', 'p'))
  val floor2 = Floor(List('t', 'r', 'c'), List('r', 'c'))
  val floor3 = Floor(List(), List('t'))
  val floor4 = Floor(List(), List())
  State(0, Array(floor1, floor2, floor3, floor4))
}

