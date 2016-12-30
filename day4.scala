val input = io.Source.fromFile("day4.txt").mkString.lines.toList

def extract(s: String): Option[Int] = {
  val pattern = """(.*)-(\d*)\[(.*)\]""".r
  s match {
    case pattern(s, secId, top5) => {
      val s2 = s.filter(_ != '-')
        .sorted
        .groupBy(identity)
        .mapValues(_.length)
        .toList
        .sortWith((x, y) => x._2 > y._2 || (x._2 == y._2 && x._1 < y._1))
        .take(5)
        .map(_._1)
        .mkString
      if (s2 == top5) Some(secId.toInt) else None
    }
  }
}

println(extract("aaaaa-bbb-z-y-x-123[abxyz]"))
println(extract("a-b-c-d-e-f-g-h-987[abcde]"))
println(extract("not-a-real-room-404[oarel]"))
println(extract("totally-real-room-200[decoy]"))
println()

println(input.map(extract).flatten.sum)
