import java.security.MessageDigest
import scala.collection.immutable.Queue

val input: String = "veumntbg"

def md5string(s: String): String = {
  val byteToHex: String = "0123456789abcdef"
  val digest = MessageDigest.getInstance("MD5").digest(s.getBytes)
  val sb = new StringBuilder()
  digest.foreach(x => {
    sb += byteToHex((x >>> 4) & 0xF)
    sb += byteToHex(x & 0xF)
  })
  sb.toString
}

case class State(
  x: Int,
  y: Int,
  path: List[Char]
) {
  def isFinished: Boolean = x == 3 && y == 3

  val pathString: String = path.reverse.mkString

  def moves: List[State] = List(
    if (isUpOpen) Some(State(x, y - 1, 'U' :: path)) else None,
    if (isDownOpen) Some(State(x, y + 1, 'D' :: path)) else None,
    if (isLeftOpen) Some(State(x - 1, y, 'L' :: path)) else None,
    if (isRightOpen) Some(State(x + 1, y, 'R' :: path)) else None
  ).flatten

  private val hash: String = md5string(input + pathString)

  private def isOpen(c: Char) = Set('b', 'c', 'd', 'e', 'f').contains(c)
  private def isUpOpen = y > 0 && isOpen(hash(0))
  private def isDownOpen = y < 3 && isOpen(hash(1))
  private def isLeftOpen = x > 0 && isOpen(hash(2))
  private def isRightOpen = x < 3 && isOpen(hash(3))
}

def solve(states: Queue[State]): String = {
  if (states.head.isFinished) states.head.pathString
  else solve(states.tail ++ states.head.moves)
}

def solve2(states: Queue[State], longest: Int): Int = {
  if (states.isEmpty) longest
  else if (states.head.isFinished)
    solve2(states.tail, states.head.pathString.length)
  else solve2(states.tail ++ states.head.moves, longest)
}

def initState = State(0, 0, List())

println(solve(Queue(initState)))
println(solve2(Queue(initState), 0))
