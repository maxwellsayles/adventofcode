val input = io.Source.fromFile("day9.txt").mkString.filter(_ != '\n')

def expand(s: String, acc: StringBuilder = new StringBuilder()): String = {
  val pattern = """^\((\d+)x(\d+)\).*""".r
  s match {
    case "" => acc.toString
    case pattern(c, t) => {
      val n = s"(${c}x${t})".size
      val p = s.drop(n).take(c.toInt)
      acc.append(p * t.toInt)
      expand(s.drop(n + c.toInt), acc)
    }
    case _ => {
      val (pre, post) = s.span(_ != '(')
      acc.append(pre)
      expand(post, acc)
    }
  }
}

def expand2(s: String): String = {
  println(s.size)
  if (s.exists(_ == '(')) expand2(expand(s)) else s
}

println(expand(input).size)
println(expand2(input).size)
