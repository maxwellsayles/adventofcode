val matcher = """(\d+)-(\d+)""".r

val input = io.Source.fromFile("day20.txt").mkString.lines.toList.map({
  case matcher(s, t) => (s.toLong, t.toLong)
}).sorted

def solve(x: Long, ranges: List[(Long, Long)]): Long = {
  val (s, t) = ranges.head
  if (x < s) x
  else solve(x.max(t + 1), ranges.tail)
}

println(solve(0, input))


def allowed(c: Long, x: Long, ranges: List[(Long, Long)]): Long = {
  if (ranges.isEmpty) c
  else {
    val (s, t) = ranges.head
    val x2 = x.max(t + 1)
    val c2 = c + 0L.max(s - x)
    allowed(c2, x2, ranges.tail)
  }
}

print(allowed(0, 0, input))
