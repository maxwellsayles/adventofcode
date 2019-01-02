val input = io.Source.fromFile("day7.txt").mkString.lines.toList



def abba(m: List[Char]): Boolean = {
  m match {
    case s::t::u::v::xs => {
      if (s == v && t == u && s != t) true
      else abba(m.tail)
    }
    case _ => false
  }
}

def aba(m: List[Char]): Set[(Char, Char)] = {
  m match {
    case s::t::u::xs => {
      if (s == u && s != t) aba(m.tail) + ((s, t)) else aba(m.tail)
    }
    case _ => Set()
  }
}

def separate(
  m: List[Char],
  xs: List[List[Char]] = List(),
  ys: List[List[Char]] = List()
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

def tls(m: String): Boolean = {
  val (xs, ys) = separate(m.toList)
  xs.exists(abba) && !ys.exists(abba)
}

def ssl(m: String): Boolean = {
  val (xs, ys) = separate(m.toList)
  val s1 = xs.map(aba)
    .foldLeft(Set[(Char, Char)]())((acc, s) => acc.union(s))
  val s2 = ys.map(aba)
    .foldLeft(Set[(Char, Char)]())((acc, s) => acc.union(s))
    .map({case (x, y) => (y, x)})
  !s1.intersect(s2).isEmpty
}

println(input.filter(tls).size)
println(input.filter(ssl).size)
