val input = io.Source.fromFile("day2.txt").mkString.lines.toList

trait Movable[T] {
  def up: T
  def down: T
  def left: T
  def right: T
}

case class Point(x: Int, y: Int) extends Movable[Point] {
  def up: Point = if (y == 0) Point(x, y) else Point(x, y-1)
  def down: Point = if (y == 2) Point(x, y) else Point(x, y+1)
  def left: Point = if (x == 0) Point(x, y) else Point(x-1, y)
  def right: Point = if (x == 2) Point(x, y) else Point(x+1, y)
  def toInt: Int = y*3 + x + 1
}

case class Point2(x: Int, y: Int) extends Movable[Point2] {
  private def isValid: Boolean = x.abs + y.abs <= 2

  def up: Point2 = { val p = Point2(x, y - 1); if (p.isValid) p else this }
  def down: Point2 = { val p = Point2(x, y + 1); if (p.isValid) p else this }
  def left: Point2 = { val p = Point2(x - 1, y); if (p.isValid) p else this }
  def right: Point2 = { val p = Point2(x + 1, y); if (p.isValid) p else this }

  def toChar: Char = "  1   234 56789 ABC   D  "(x + 2 + (y + 2) * 5)
}

def step[T <: Movable[T]](acc: T, d: Char): T = d match {
  case 'U' => acc.up
  case 'D' => acc.down
  case 'L' => acc.left
  case 'R' => acc.right
}

def nextCode[T <: Movable[T]](acc: T, l: String): T = l.foldLeft(acc)(step)

val sol = input.scanLeft(Point(1, 1))(nextCode).toList.tail.map(_.toInt)
sol.foreach(print(_))
println()

val sol2 = input.scanLeft(Point2(0, 0))(nextCode).toList.tail.map(_.toChar)
sol2.foreach(print(_))
println()

