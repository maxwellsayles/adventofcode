def popCount(x: Int): Int = {
  val x0 = (x & 0x55555555) + ((x >> 1) & 0x55555555);
  val x1 = (x0 & 0x33333333) + ((x0 >> 2) & 0x33333333);
  val x2 = (x1 & 0x0F0F0F0F) + ((x1 >> 4) & 0x0F0F0F0F);
  val x3 = (x2 & 0x00FF00FF) + ((x2 >> 8) & 0x00FF00FF);
  (x3 & 0x0000FFFF) + ((x3 >>16) & 0x0000FFFF);
}

def isOpen(x: Int, y: Int): Boolean = {
  if (x < 0 || y < 0)
    false
  else {
    val v = x*x + 3*x + 2*x*y + y + y*y + 1364
    val w = popCount(v)
    (w & 1) == 0
  }
}

def moves(x: Int, y: Int): List[(Int, Int)] = {
  def isOkay(x: Int, y: Int): Option[(Int, Int)] =
    if (isOpen(x, y)) Some((x, y)) else None

  List(isOkay(x - 1, y), isOkay(x + 1, y), isOkay(x, y - 1), isOkay(x, y + 1))
    .flatten
}

case class State(
  stepCount: Int,
  positions: Set[(Int, Int)],
  visited: Set[(Int, Int)]
) {
  def isFinished =
    positions.exists(p => p._1 == 31 && p._2 == 39)

  def step = {
    val newVisited = visited ++ positions
    val newPositions = positions.flatMap(p => moves(p._1, p._2).toSet) -- visited
    State(stepCount + 1, newPositions, newVisited)
  }
}

def solve(state: State): Int = {
  if (state.isFinished)
    state.stepCount
  else
    solve(state.step)
}

// Start at 1, 1. Go to 31, 39.
val initState = State(0, Set((1, 1)), Set())
println(solve(initState))
