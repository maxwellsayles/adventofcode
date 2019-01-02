val input = io.Source.fromFile("day4.txt").mkString.lines.toList

def extract(s: String): Option[(String, Int)] = {
  val pattern = """(.*)-(\d*)\[(.*)\]""".r
  s match {
    case pattern(s, secId, top5) => {
      val s2 = s.filter(_ != '-')
        .groupBy(identity)
        .mapValues(_.length)
        .toList
        .sortWith((x, y) => x._2 > y._2 || (x._2 == y._2 && x._1 < y._1))
        .take(5)
        .map(_._1)
        .mkString
      if (s2 == top5) Some((s, secId.toInt)) else None
    }
  }
}

val xs = input.map(extract).flatten

println(extract("aaaaa-bbb-z-y-x-123[abxyz]"))
println(extract("a-b-c-d-e-f-g-h-987[abcde]"))
println(extract("not-a-real-room-404[oarel]"))
println(extract("totally-real-room-200[decoy]"))

println("Part 1: " + xs.map(_._2).sum)
println()

def rotate(c: Char, x: Int): Char = 
  if (c == '-') ' ' else ((((c - 'a') + x) % 26) + 'a').toChar

def rotate(s: String, x: Int): String =
  s.map(rotate(_, x))

println(rotate("qzmt-zixmtkozy-ivhz", 343))

val sol = xs.filter(v => rotate(v._1, v._2) == "northpole object storage").head
println("Part 2: " + sol._2)
