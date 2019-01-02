val initDiscs: List[(Int, Int)] =
//  List((5, 2), (13, 7), (17, 10), (3, 2), (19, 9), (7, 0))
  List((5, 2), (13, 7), (17, 10), (3, 2), (19, 9), (7, 0), (11, 0))

val (initSlots, initPositions) = initDiscs.unzip

val offsetPositions: List[Int] = {
  initPositions.zipWithIndex
    .map({ case (e, i) => e + i + 1 })
    .zip(initSlots)
    .map({ case (p, s) => p % s })
}

val initSlotsWithOffsets = (initSlots, offsetPositions).zipped.toList

def calcAddum(value: Int, delta: Int, discs: List[(Int, Int)]): Int = {
  if (discs.isEmpty) value
  else {
    val (slots, position) = discs.head
    if ((value + position) % slots ==  0)
      calcAddum(value, delta * slots, discs.tail)
    else
      calcAddum(value + delta, delta, discs)
  }
}

def solve(discs: List[(Int, Int)]): Int = {
  val (firstSlots, firstPosition) = discs.head
  calcAddum(firstSlots - firstPosition, firstSlots, discs.tail)
}

print(solve(initSlotsWithOffsets))
