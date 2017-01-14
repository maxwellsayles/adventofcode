val input = io.Source.fromFile("day6.txt").mkString.lines.toList

def histogram[A, S <% Seq[A]](xs: S): Map[A, Int] = {
  def helper(acc: Map[A, Int], x: A): Map[A, Int] = {
    if (acc.contains(x)) acc + (x -> (acc(x) + 1))
    else acc + (x -> 1)
  }
  xs.foldLeft(Map[A, Int]())(helper)
}

def most[A, S <% Seq[A]](xs: S): A = {
  histogram(xs).toList.sortBy(_._2).reverse.head._1
}

def transpose(xs: List[String]): Seq[String] = {
  0.to(7).map(i => xs.map(_(i)).mkString)
}

println(transpose(input).map(most(_)).mkString)

