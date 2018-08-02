val input = io.Source.fromFile("day22.txt").mkString.lines

case class Stat(size: Int, used: Int)

val dfRegex = """/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T.*""".r
val grid = input.foldLeft(Map[(Int, Int), Stat]())({
  case (acc, dfRegex(x, y, size, used, _)) => {
    acc + ((x.toInt, y.toInt) -> Stat(size.toInt, used.toInt))
  }
})

val maxx = grid.keys.map(_._1).max
val maxy = grid.keys.map(_._2).max

def isViable(sx: Int, sy: Int, tx: Int, ty: Int): Boolean = {
    val Stat(tsize, tused) = grid(tx, ty)
    tused + grid(sx, sy).used <= tsize
}

def isNeighbor(sx: Int, sy: Int, tx: Int, ty: Int): Boolean = {
  (((sx == tx + 1 || sx == tx - 1) && sy == ty) ||
    ((sy == ty + 1 || sy == ty - 1) && sx == tx))
}

val pairs = for (
  sx <- 0 to maxx;
  sy <- 0 to maxy;
  tx <- 0 to maxx;
  ty <- 0 to maxy;
  if grid(sx, sy).used != 0 && !(sx == tx && sy == ty) && isViable(sx, sy, tx, ty) && isNeighbor(sx, sy, tx, ty))
    yield (sx, sy, tx, ty)

//println(pairs.size)

pairs.foreach(println)


// Track payload and empty block.
// Move neighbors into empty block.
// Use a SortedSet for the state queue.
// dist(payload, origin) is priority, then dist(empty, payload), then dist(empty, origin), then x, then y
