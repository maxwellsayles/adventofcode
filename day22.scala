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

val pairs = for (
  sx <- 0 to maxx;
  sy <- 0 to maxy;
  tx <- 0 to maxx;
  ty <- 0 to maxy;
  if initGrid(sx, sy).used != 0 && !(sx == tx && sy == ty) && isViable(sx, sy, tx, ty))
    yield (sx, sy, tx, ty)

println
println(pairs.size)

// Track payload and empty block.
// Move neighbors into empty block.
// Use a SortedSet for the state queue.
// dist(payload, origin) is priority, then dist(empty, payload), then dist(empty, origin), then x, then y
