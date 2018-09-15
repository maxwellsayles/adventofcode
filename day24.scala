val input = io.Source.fromFile("day24.txt").mkString.lines.toArray

case class State(x: Int, y: Int, bag: Int) {
  def isFinished: Boolean = bag == 255

  private def moveTo(nx: Int, ny: Int): Option[State] = {
    val c = input(ny)(nx)
    if (c == '#') {
      None
    } else if (c == '.') {
      Some(State(nx, ny, bag))
    } else {
      val newBag = bag | (1 << c.asDigit)
      Some(State(nx, ny, newBag))
    }
  }

  def moves: List[State] = {
    List(moveTo(x - 1, y), moveTo(x + 1, y), moveTo(x, y - 1), moveTo(x, y + 1))
      .flatten
  }
}

val initState: State = {
  val tmp = input.map(line => line.indexOf('0'))
  val x = tmp.filter(x => x != -1).head
  val y = tmp.takeWhile(x => x == -1).length
  State(x, y, 1)
}

case class Solver(
  inputs: List[State],
  outputs: List[State],
  stepCount: Int,
  visited: Set[State]
) {
  def isFinished: Boolean = !inputs.isEmpty && inputs.head.isFinished

  def step: Solver = {
    if (inputs.isEmpty) {
      Solver(outputs, List(), stepCount + 1, visited)
    } else {
      val state = inputs.head
      val moves = state.moves.filter(!visited(_))
      Solver(inputs.tail, moves ::: outputs, stepCount, visited ++ moves)
    }
  }
}

def initSolver = Solver(List(initState), List(), 0, Set(initState))

def solve(solver: Solver): Int = {
  if (solver.isFinished) {
    solver.stepCount
  } else {
    solve(solver.step)
  }
}

println(solve(initSolver))
