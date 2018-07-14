import scala.collection.mutable.HashMap

val input: Int = 3017957
//val input: Int = 5

def solve(i: Int, j: Int, k: Int): Int = {
  if (i == j) i
  else {
    val d = (i - j) / k;
    if (d % 2 == 0) solve(i + 2 * k, j, 2 * k)
    else solve(i, j - k, 2 * k)
  }
}

println(solve(1, input, 1))

case class State(
  nodes: Map[Int, Int],
  taker: Int,
  giverPrev: Int,
  giver: Int,
  dist: Int
) {
  def isFinished: Boolean = nodes.size == 1
  def result: Int = nodes.keys.head
  def size = nodes.size
  def nextTaker: State = State(nodes, nodes(taker), giverPrev, giver, dist - 1)
  def nextGiver: State = State(nodes, taker, giver, nodes(giver), dist + 1)
  def removeGiver: State = {
    val giverNext = nodes(giver)
    val newNodes = nodes - giver + (giverPrev -> giverNext)
    State(newNodes, taker, giverPrev, giverNext, dist)
  }
}

def advanceGiverToDist(state: State, targetDist: Int): State = {
  if (state.dist == targetDist) state
  else advanceGiverToDist(state.nextGiver, targetDist)
}

val initNodes = {
  val m = HashMap[Int, Int]()
  0.until(input).foreach(x => m += x -> (x + 1) % input)
  m.toMap
}

val initState = State(initNodes, 0, 0, 1, 1)

def solve2(state: State): Int = {
  if (state.isFinished) state.result
  else solve2(advanceGiverToDist(state, state.size / 2).removeGiver.nextTaker)
}

println(solve2(initState) + 1)
