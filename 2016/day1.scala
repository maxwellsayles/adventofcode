val input = {
  val txt = "R3, R1, R4, L4, R3, R1, R1, L3, L5, L5, L3, R1, R4, L2, L1, R3, L3, R2, R1, R1, L5, L2, L1, R2, L4, R1, L2, L4, R2, R2, L2, L4, L3, R1, R4, R3, L1, R1, L5, R4, L2, R185, L2, R4, R49, L3, L4, R5, R1, R1, L1, L1, R2, L1, L4, R4, R5, R4, L3, L5, R1, R71, L1, R1, R186, L5, L2, R5, R4, R1, L5, L2, R3, R2, R5, R5, R4, R1, R4, R2, L1, R4, L1, L4, L5, L4, R4, R5, R1, L2, L4, L1, L5, L3, L5, R2, L5, R4, L4, R3, R3, R1, R4, L1, L2, R2, L1, R4, R2, R2, R5, R2, R5, L1, R1, L4, R5, R4, R2, R4, L5, R3, R2, R5, R3, L3, L5, L4, L3, L2, L2, R3, R2, L1, L1, L5, R1, L3, R3, R4, R5, L3, L5, R1, L3, L5, L5, L2, R1, L3, L1, L3, R4, L1, R3, L2, L2, R3, R3, R4, R4, R1, L4, R1, L5"
  // val txt = "R8, R4, R4, R8"
  val xs = txt.split(", ")
  xs.map(_.head) zip xs.map(_.tail.toInt)
}

case class Direction(x: Int, y: Int) {
  def left = Direction(y, -x)
  def right = Direction(-y, x)
  def *(m: Int) = Direction(x * m, y * m)
}

case class Point(x: Int, y: Int) {
  def +(dir: Direction): Point = Point(x + dir.x, y + dir.y)
}

def step: ((Direction, Point), (Char, Int)) => (Direction, Point) = {
  case ((dir, cur), (turn, length)) =>
    turn match {
      case 'L' => (dir.left, cur + dir.left * length)
      case 'R' => (dir.right, cur + dir.right * length)
    }
}

val solution = input.foldLeft((Direction(0, 1), Point(0, 0)))(step)
val p = solution._2
println(p.x.abs + p.y.abs)

def moveTowards(p: Point, q: Point): Point = {
  if (p.x == q.x && p.y < q.y) Point(p.x, p.y + 1)
  else if (p.x == q.x && p.y > q.y) Point(p.x, p.y - 1)
  else if (p.x < q.x) Point(p.x + 1, p.y)
  else Point(p.x - 1, p.y)
}

def interp(p: Point, xs: List[Point], acc: Set[Point]): Point = {
  if (acc.contains(p)) p else {
    val hd :: tl = xs
    if (p == hd) interp(p, tl, acc)
    else interp(moveTowards(p, hd), xs, acc + p)
  }
}

val visits = input.scanLeft((Direction(0, 1), Point(0, 0)))(step).map(_._2)
val p2 = interp(Point(0, 0), visits.toList.tail, Set())
println(p2.x.abs + p2.y.abs)

