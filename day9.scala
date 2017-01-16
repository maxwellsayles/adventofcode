val input = io.Source.fromFile("day9.txt").mkString.filter(_ != '\n')

def expand(s: String, acc: String = ""): String = {
  val pattern = """^\((\d+)x(\d+)\).*""".r
  s match {
    case pattern(c, t) => {
      val n = s"(${c}x${t})".size
      val p = s.drop(n).take(c.toInt)
      expand(s.drop(n + c.toInt), acc + p * t.toInt)
    }
    case "" => acc
    case _ => expand(s.tail, acc + s.head)
  }
}

println(expand(input, "").size)
