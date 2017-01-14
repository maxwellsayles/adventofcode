val input = io.Source.fromFile("day7.txt").mkString.lines.toList



def abba(m: List[Char]): Boolean = {
  m match {
    case s::t::u::v::xs => {
      if (s == v && t == u && s != t && s.isLetter && t.isLetter) true
      else abba(t::u::v::xs)
    }
    case _ => false
  }
}

def abbas(m: String): Boolean = abba(m.toList)

def separate(
  m: List[Char],
  xs: List[List[Char]],
  ys: List[List[Char]]
): (List[List[Char]], List[List[Char]]) = {
  if (m.isEmpty) (xs, ys)
  else {
    val (x, rs) = m.span(_ != '[')
    val xs2 = x :: xs
    if (rs.isEmpty) (xs2, ys)
    else {
      val (ss, ts) = rs.span(_ != ']')
      val ys2 = ss.tail :: ys
      if (ts.isEmpty) (xs2, ys2)
      else separate(ts.tail, xs2, ys2)
    }
  }
}

def separateStr(m: String): (List[String], List[String]) = {
  val (xs, ys) = separate(m.toList, List(), List())
  (xs.map(_.mkString), ys.map(_.mkString))
}

def tls(m: String): Boolean = {
  val (xs, ys) = separate(m.toList, List(), List())
  xs.exists(abba) && !ys.exists(abba)
}

println(input.filter(tls).size)
