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
  nodes: Map[Int, (Int, Int)],
  taker: Int,
  giver: Int,
  dist: Int
) {
  def isFinished: Boolean = nodes.size == 1
  def result: Int = nodes.keys.head
  def size = nodes.size
  def nextTaker: State = State(nodes, nodes(taker)._2, giver, dist - 1)
  def nextGiver: State = State(nodes, taker, nodes(giver)._2, dist + 1)
  def removeGiver: State = {
    val (before, after) = nodes(giver)
    val newAfter = (before, nodes(after)._2)
    val newBefore = (nodes(before)._1, after)
    val newNodes = nodes - giver + (after -> newAfter) + (before -> newBefore)
    State(newNodes, taker, after, dist)
  }
}

def advanceGiverToDist(state: State, targetDist: Int): State = {
  if (state.dist == targetDist) state
  else advanceGiverToDist(state.nextGiver, targetDist)
}

val initNodes = {
  val m = HashMap[Int, (Int, Int)]()
  0.until(input).foreach(x => m += x -> ((x + input - 1) % input, (x + 1) % input))
  m.toMap
}

val initState = State(initNodes, 0, 0, 0)

def solve2(state: State): Int = {
  if (state.isFinished) state.result
  else solve2(advanceGiverToDist(state, state.size / 2).removeGiver.nextTaker)
}

println(solve2(initState) + 1)
