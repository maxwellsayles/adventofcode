val input = io.Source.fromFile("day21.txt").mkString.lines.toList
val start: String = "abcdefgh"

val swapPosRegex = """swap position (\d+) with position (\d+)""".r
val swapCharRegex = """swap letter (\w) with letter (\w)""".r
val rotLeftRegex = """rotate left (\d+) steps?""".r
val rotRightRegex = """rotate right (\d+) steps?""".r
val rotCharRegex = """rotate based on position of letter (\w)""".r
val revRegex = """reverse positions (\d+) through (\d+)""".r
val moveRegex = """move position (\d+) to position (\d+)""".r

def step(s: String, action: String): String = {
  action match {
    case swapPosRegex(x, y) => {
      val xi = x.toInt
      val yi = y.toInt
      s.updated(xi, s(yi)).updated(yi, s(xi))
    }

    case swapCharRegex(x, y) => {
      val xi = s.indexOf(x)
      val yi = s.indexOf(y)
      s.updated(xi, s(yi)).updated(yi, s(xi))
    }

    case rotLeftRegex(i) => {
      val j = i.toInt % s.length
      s.drop(j) + s.take(j)
    }

    case rotRightRegex(i) => {
      val j = (s.length - i.toInt) % s.length
      s.drop(j) + s.take(j)
    }

    case rotCharRegex(c) => {
      val i = s.indexOf(c)
      val j = 1 + i + (if (i >= 4) 1 else 0)
      val k = (2 * s.length - j) % s.length
      s.drop(k) + s.take(k)
    }

    case revRegex(x, y) => {
      val xi = x.toInt
      val yi = y.toInt
      s.take(xi) + s.drop(xi).take(yi - xi + 1).reverse + s.drop(yi + 1)
    }

    case moveRegex(x, y) => {
      val xi = x.toInt
      val yi = y.toInt
      val c = s(xi)
      val (a, b) = (s.take(xi) + s.drop(xi + 1)).splitAt(yi)
      a + c + b
    }
  }
}

println(input.foldLeft(start)(step))
