import scala.collection.SeqLike
import scala.collection.Seq
import Numeric._

val input = io.Source.fromFile("day3.txt")
  .mkString.lines.toList.map(_.split(" ").filter(_.length > 0).map(_.toInt))

def isValid[N, S <% Seq[N]](xs: S)(implicit n: Numeric[N]): Boolean = {
  val Seq(x, y, z) = xs.sorted
  n.gt(n.plus(x, y), z)
}

val valids = input.filter(isValid[Int, Array[Int]])
println(valids.length)

def transpose[T, S <% Seq[T]](xs: List[S]): List[List[T]] =
  if (xs(0).isEmpty) List() else xs.map(_.head) :: transpose(xs.map(_.tail))

val shuffled = input.grouped(3).map(transpose[Int, Array[Int]]).flatten
println(shuffled.filter(isValid[Int, List[Int]]).length)
