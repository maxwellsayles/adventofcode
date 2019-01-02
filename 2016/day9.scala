val input = io.Source.fromFile("day9.txt").mkString.filter(_ != '\n')

// This worked fine for expanding once.
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

// This can OOM when things get very large.
def expand2(s: String): String = {
  println(s.size)
  if (s.exists(_ == '(')) expand2(expand(s)) else s
}

// This just returns the size without actually expanding anything.
def expsize(s: String, acc: Int = 0): Int = {
  val pattern = """^\((\d+)x(\d+)\).*""".r
  s match {
    case "" => acc
    case pattern(c, t) => {
      val n = c.size + t.size + 3
      val acc2 = acc + c.toInt * t.toInt
      expsize(s.drop(n + c.toInt), acc2)
    }
    case _ => {
      val (pre, post) = s.span(_ != '(')
      expsize(post, acc + pre.size)
    }
  }
}

// This computes the size of recursively expanding inner strings, without
// actually expanding anything.
def expsize2(s: String, acc: Long = 0): Long = {
  val pattern = """^\((\d+)x(\d+)\).*""".r
  s match {
    case "" => acc
    case pattern(c, t) => {
      val n = c.size + t.size + 3
      val inner = s.drop(n).take(c.toInt)
      val expInnerSize = expsize2(inner)
      val acc2 = acc + expInnerSize * t.toInt
      expsize2(s.drop(n + c.toInt), acc2)
    }
    case _ => {
      val (pre, post) = s.span(_ != '(')
      expsize2(post, acc + pre.size)
    }
  }
}

println(expsize(input))
println(expsize2(input))
