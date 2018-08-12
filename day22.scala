val input = io.Source.fromFile("day22.txt").mkString.lines

case class NodeStat(size: Int, used: Int)

val dfRegex = """/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T.*""".r
val initGrid = input.foldLeft(Map[(Int, Int), NodeStat]())({
  case (acc, dfRegex(x, y, size, used, _)) => {
    acc + ((x.toInt, y.toInt) -> NodeStat(size.toInt, used.toInt))
  }
})

val maxx = initGrid.keys.map(_._1).max
val maxy = initGrid.keys.map(_._2).max

def isViable(sx: Int, sy: Int, tx: Int, ty: Int): Boolean = {
    val NodeStat(tsize, tused) = initGrid(tx, ty)
    tused + initGrid(sx, sy).used <= tsize
}

def isValidPos(x: Int, y: Int): Boolean =
  x >=0 && x <= maxx && y >= 0 && y <= maxx

val pairs = for (
  sx <- 0 to maxx;
  sy <- 0 to maxy;
  tx <- 0 to maxx;
  ty <- 0 to maxy;
  if initGrid(sx, sy).used != 0 && !(sx == tx && sy == ty) && isViable(sx, sy, tx, ty))
    yield (sx, sy, tx, ty)

println
println(pairs.size)

//println(pairs.size)

// Track payload and empty block.
// Move neighbors into empty block.
// Use a SortedSet for the state queue.
// dist(payload, origin) is priority, then dist(empty, payload), then dist(empty, origin), then x, then y

case class GridState(
  grid: Map[(Int, Int), NodeStat],
  goalPos: (Int, Int),
  emptyPos: (Int, Int),
  steps: Int
) {
  def isFinished: Boolean = goalPos._1 == 0 && goalPos._2 == 0

  def isViable(sx: Int, sy: Int, tx: Int, ty: Int): Boolean = {
    val NodeStat(tsize, tused) = grid(tx, ty)
    tused + grid(sx, sy).used <= tsize
  }

  private def canMove(sx: Int, sy: Int, tx: Int, ty: Int): Boolean =
    isValidPos(sx, sy) && isValidPos(tx, ty) && isViable(sx, sy, tx, ty)

  private def move(sx: Int, sy: Int, tx: Int, ty: Int): Option[GridState] = {
    if (canMove(sx, sy, tx, ty)) {
      val NodeStat(sUsed, sSize) = grid(sx, sy)
      val NodeStat(tUsed, tSize) = grid(tx, ty)
      val newGrid =
        grid + ((sx, sy) -> NodeStat(sSize, 0)) + ((tx, ty) -> NodeStat(tSize, sUsed + tUsed))
      if (goalPos == (sx, sy)) {
        Some(GridState(newGrid, (tx, ty), (sx, sy), steps + 1))
      } else {
        Some(GridState(newGrid, goalPos, (sx, sy), steps + 1))
      }
    } else {
      None
    }
  }

  def nextStates: List[GridState] = {
    val emptyX = emptyPos._1
    val emptyY = emptyPos._2
    List(
      (emptyX - 1, emptyY),
      (emptyX + 1, emptyY),
      (emptyX, emptyY - 1),
      (emptyX, emptyY + 1)
    )
      .map({ case (x, y) => move(x, y, emptyX, emptyY) })
      .flatten
  }
}

case class SolverState(
  visited: Set[(Int, Int)]
)

val initGoalPos = (maxx, 0)
val initEmptyPos =
  (for (
    x <- 0 to maxx;
    y <- 0 to maxy;
    if initGrid(x, y).used == 0
  ) yield (x, y)).head
val initGridState = GridState(initGrid, initGoalPos, initEmptyPos, 0)

