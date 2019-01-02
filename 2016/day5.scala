val input = "uqwqemis"
val md5 = java.security.MessageDigest.getInstance("md5")

def code(i: Int): Option[(Int, Char)] = {
  val xs = md5.digest((input + i.toString).getBytes)
  if (xs(0) == 0 && xs(1) == 0 && xs(2) < 0x10 && xs(2) >= 0) {
    Some((xs(2), "%02x".format(xs(3))(0)))
  } else {
    None
  }
}

def next(s: String, i: Int): String = {
  if (s.size == 8) s
  else {
    code(i) match {
      case Some((x, y)) => {
        val c = "0123456789ABCDEF"(x)
        next(s + c, i + 1)
      }
      case None => next(s, i + 1)
    }
  }
}

println(next("", 0))

def next2(m: Map[Int, Char], i: Int): String = {
  if (m.size == 8) 0.to(7).map(i => m(i)).mkString
  else {
    code(i) match {
      case Some((x, y)) => {        
        if (x > 7 || m.contains(x)) next2(m, i + 1)
        else next2(m + (x -> y), i + 1)
      }
      case None => next2(m, i + 1)
    }
  }
}

println(next2(Map(), 0))
