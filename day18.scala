val input = "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^.."
val inputRowCount = 40

def nextRow(row: String): String = {
  ("." + row + ".")
    .tails
    .map(_.take(3))
    .filter(x => x.length == 3)
    .map({
      case "^^." => '^'
      case ".^^" => '^'
      case "^.." => '^'
      case "..^" => '^'
      case _ => '.'
    })
    .mkString
}

def room(row: String): Stream[String] = row #:: room(nextRow(row))

val solution = room(input).take(inputRowCount).map(x => x.count(_ == '.')).sum
