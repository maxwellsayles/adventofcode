val input = "uqwqemis"
val md5 = java.security.MessageDigest.getInstance("md5")

def code(i: Int): Option[Char] = {
  val xs = md5.digest((input + i.toString).getBytes)
  if (xs(0) == 0 && xs(1) == 0 && xs(2) < 0x10 && xs(2) >= 0) {
    Some("0123456789ABCDEF"(xs(2)))
  } else {
    None
  }
}

def next(l: Int, i: Int): Unit = {
  if (l > 0) {
    code(i) match {
      case Some(x) => {
        print(x)
        next(l - 1, i + 1)
      }
      case None => {
        next(l, i + 1)
      }
    }
  }
}
