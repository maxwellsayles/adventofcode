sealed trait Command
case class Rect(w: Int, h: Int) extends Command
case class RotateCol(x: Int, by: Int) extends Command
case class RotateRow(y: Int, by: Int) extends Command

class MatchError(obj: Any) extends RuntimeException

def parse(s: String): Command = {
  val rectPattern = """rect (\d+)x(\d+)""".r
  val colPattern = """rotate column x=(\d+) by (\d+)""".r
  val rowPattern = """rotate row y=(\d+) by (\d+)""".r
  s match {
    case rectPattern(w, h) => Rect(w.toInt, h.toInt)
    case colPattern(x, by) => RotateCol(x.toInt, by.toInt)
    case rowPattern(y, by) => RotateRow(y.toInt, by.toInt)
    case _ => throw new MatchError("Unrecognized input " + s)
  }
}

val input = io.Source.fromFile("day8.txt").mkString.lines.map(parse).toList
val n = 50

type Screen = Array[Array[Boolean]]

def rect(r: Rect, arr: Screen) {
  for (x <- 0 until r.w) {
    for (y <- 0 until r.h) {
      arr(x)(y) = true
    }
  }
}

def rotateRow(r: RotateRow, arr: Screen) {
  val tmp = Array.ofDim[Boolean](n)
  for (x <- 0 until n) {
    tmp(x) = arr(x)(r.y)
  }
  for (x <- 0 until n) {
    arr(x)(r.y) = tmp((x - r.by + n) % n)
  }
}

def rotateCol(r: RotateCol, arr: Screen) {
  val tmp = Array.ofDim[Boolean](n)
  for (y <- 0 until n) {
    tmp(y) = arr(r.x)(y)
  }
  for (y <- 0 until n) {
    arr(r.x)(y) = tmp((y - r.by + n) % n)
  }
}

def apply(c: Command, arr: Screen) {
  c match {
    case Rect(w, h) => rect(Rect(w, h), arr)
    case RotateCol(x, by) => rotateCol(RotateCol(x, by), arr)
    case RotateRow(y, by) => rotateRow(RotateRow(y, by), arr)
  }
}

val screen = Array.ofDim[Boolean](n, n)
input.foreach(c => apply(c, screen))
println(screen.map(_.count(identity)).sum)
