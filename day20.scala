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
