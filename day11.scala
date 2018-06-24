sealed abstract trait Part { def name: Char }

case class Generator(name: Char) extends Part {
  override def toString: String = name.toUpper.toString
}

case class Chip(name: Char) extends Part {
  override def toString: String = name.toLower.toString
}

case class Floor(parts: List[Part]) {
  override def toString: String = parts.mkString("")

  lazy val generators: List[Generator] = 
    for (Generator(x) <- parts) yield Generator(x)

  lazy val chips: List[Chip] = 
    for (Chip(x) <- parts) yield Chip(x)

  lazy val isValid: Boolean =
    generators.isEmpty ||
    chips.forall(c => generators.contains(Generator(c.name)))

  def isEmpty: Boolean = parts.isEmpty

  def moveOne: List[(List[Part], Floor)] =
    removeOne(parts).map({
      case (x, xs) => (List(x), Floor(xs))
    }).filter(_._2.isValid)

  def moveTwo: List[(List[Part], Floor)] =
    moveOne.flatMap({
      case (List(x), Floor(xs)) =>
        removeOne(xs).map({
          case (y, ys) => {
            val elevatorParts = if (x.name < y.name) List(x, y) else List(y, x)
            (elevatorParts, Floor(ys))
          }
        })
    }).filter(_._2.isValid).distinct
}

// Represents and equivalence class of states, i.e. the elevator location and
// the floor number for pairs of generators and chips, agnostic of their names.
case class NormalizedState(
  elevator: Int,
  pairs: List[(Int, Int)]
)

// Represents the state of the puzzle, i.e. all floors and elevator location
case class State(  
  elevator: Int,
  floors: Array[Floor]
) {
  override def toString: String = {
    val floorStr = floors.mkString(", ")
    s"State(${elevator}, ${floorStr})"
  }

  def isValid: Boolean = floors.forall(_.isValid)

  def isFinished: Boolean = floors.take(3).forall(_.isEmpty)

  // The normalized state is the set of pairs matching chips and generators by
  // floor.
  lazy val normalized: NormalizedState = {
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

  def updated(
    newFloorNumber: Int,
    elevatorParts: List[Part],
    oldFloorNumber: Int,
    updatedOldFloor: Floor
  ): State = {
    val newParts = floors(newFloorNumber).parts ::: elevatorParts
    val newFloors = floors
      .updated(newFloorNumber, Floor(newParts))
      .updated(oldFloorNumber, updatedOldFloor)
    State(newFloorNumber, newFloors)
  }

  def moved(
    moves: List[(List[Part], Floor)],
    dstFloorNumber: Int,
    srcFloorNumber: Int
  ): List[State] = moves.map({
    case (elevatorParts, floor) =>
      updated(dstFloorNumber, elevatorParts, srcFloorNumber, floor)
  })
}

// Represents the state of the solver.
case class Solver(
  stepCount: Int,
  inputs: List[State],
  outputs: List[State],
  visited: Set[NormalizedState]
) {
  def isFinished: Boolean = !inputs.isEmpty && inputs.head.isFinished

  def step: Solver =
    if (inputs.isEmpty)
      Solver(stepCount + 1, outputs, List(), visited)
    else {
      val hd = inputs.head
      val normalized = hd.normalized
      if (visited.contains(normalized))
        Solver(stepCount, inputs.tail, outputs, visited)
      else {
        val floor = hd.floors(hd.elevator)
        val moves = floor.moveOne ::: floor.moveTwo
        val nextStates = hd.elevator match {
          case 0 => hd.moved(moves, 1, 0)
          case 1 => hd.moved(moves, 0, 1) ::: hd.moved(moves, 2, 1)
          case 2 => hd.moved(moves, 1, 2) ::: hd.moved(moves, 3, 2)
          case 3 => hd.moved(moves, 2, 3)
        }
        val validNextStates = nextStates
          .filter(_.isValid)
          .filter(state => !visited.contains(state.normalized))

        val newOutputs = outputs ::: validNextStates
        val newVisited = visited + normalized
        Solver(stepCount, inputs.tail, newOutputs, newVisited)
      }
    }
}

def removeOne[T](xs: List[T]): List[(T, List[T])] =
  xs.zip(
    xs.inits.toList.reverse.init
      .zip(xs.tails.toList.tail)
      .map({case (x, y) => x ::: y})
  )

// This is the example input
// def initState: State = {
//   val floor1 = Floor(List(Chip('h'), Chip('l')))
//   val floor2 = Floor(List(Generator('h')))
//   val floor3 = Floor(List(Generator('l')))
//   val floor4 = Floor(List())
//   State(0, Array(floor1, floor2, floor3, floor4))
// }

// This is the puzzle input.
def initState: State = {
  val floor1 = Floor(List(Generator('s'), Generator('p'), Chip('s'), Chip('p')))
  val floor2 = Floor(List(Generator('t'), Generator('r'), Chip('r'), Generator('c'), Chip('c')))
  val floor3 = Floor(List(Chip('t')))
  val floor4 = Floor(List())
  State(0, Array(floor1, floor2, floor3, floor4))
}

def solve(solver: Solver): Int = {
  if (solver.isFinished)
    solver.stepCount
  else
    solve(solver.step)
}

val initSolver = Solver(0, List(initState), List(), Set())
println(solve(initSolver))

