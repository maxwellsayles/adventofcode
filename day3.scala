val input = io.Source.fromFile("day3.txt")
  .mkString.lines.toList.map(_.split(" ").filter(_.length > 0).map(_.toInt).toList)

def isValid(xs: List[Int]): Boolean = {
  val List(x, y, z) = xs.sorted
  x + y > z
}

val valids = input.filter(isValid)
println(valids.length)


def transpose[T](xs: List[List[T]]): List[List[T]] =
  if (xs(0).isEmpty) List() else xs.map(_.head) :: transpose(xs.map(_.tail))

val shuffled = input.grouped(3).map(transpose).flatten
println(shuffled.filter(isValid).length)
