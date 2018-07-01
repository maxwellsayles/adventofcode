import scala.collection.immutable.Queue

val input = "yjdafjpo"
//val input = "abc"

val md5Instance = java.security.MessageDigest.getInstance("MD5")

def md5(bs: Array[Byte]): Array[Byte] = {
  md5Instance.reset()
  md5Instance.update(bs)
  md5Instance.digest
}

case class MD5String(private val v: String) {
  lazy val s: String = md5(v.getBytes).map(x => "%02x".format(x)).mkString

  lazy val has3: Option[Char] = {
    if (s.isEmpty) None
    def loop(i: Int, c: Char, cs: List[Char]): Option[Char] =
      if (i == 3) Some(c)
      else if (cs.isEmpty) None
      else if (cs.head == c) loop(i + 1, c, cs.tail)
      else loop(1, cs.head, cs.tail)
    loop(1, s.head, s.toList.tail)
  }

  lazy val fives: Set[Char] = {
    if (s.isEmpty) Set()
    val cs = s.toList
    cs.tail.foldLeft((1, cs.head, Set[Char]()))({
      case ((ii, cc, ss), c) => {
        if (ii == 4 && c == cc) (ii + 1, cc, ss + cc)
        else if (c == cc) (ii + 1, cc, ss)
        else (1, c, ss)
      }
    })._3
  }
}

case class Solver(md5s: Queue[MD5String], i: Int, n: Int) {

  def isFinished: Boolean = isValidKey && n == 63

  val isValidKey: Boolean = {
    val candidate = md5s.head
    val rest = md5s.tail
    candidate.has3 match {
      case None => false
      case Some(c) => rest.exists(j => j.fives.contains(c))
    }
  }

  def step: Solver = {
    val newMD5s = md5s.tail :+ MD5String(input + (i + 1001).toString)
    Solver(newMD5s, i + 1, if (isValidKey) (n + 1) else n)
  }
}

val initMD5s = 0.to(1000).
  foldLeft(Queue[MD5String]())((q, x) => q :+ MD5String(input + x.toString))
val initSolver = Solver(initMD5s, 0, 0)

def solve(solver: Solver): Int = {
  if (solver.isFinished) {
    solver.i
  } else {
    solve(solver.step)
  }
}

println(solve(initSolver))
