sealed abstract trait Target
case class Output(x: Int) extends Target
case class Bot(x: Int) extends Target

sealed abstract trait Command
case class Assn(v: Int, b: Int) extends Command
case class Move(b: Int, l: Target, h: Target) extends Command

def parse(s: String): Command = {
  val assnPattern = """value (\d+) goes to bot (\d+)""".r
  val movePattern =
    """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r
  s match {
    case assnPattern(v, b) => Assn(v.toInt, b.toInt)
    case movePattern(b, lt, lx, ht, hx) => {
      val l = if (lt == "bot") Bot(lx.toInt) else Output(lx.toInt)
      val h = if (ht == "bot") Bot(hx.toInt) else Output(hx.toInt)
      Move(b.toInt, l, h)
    }
  }
}

val input = io.Source.fromFile("day10.txt").mkString.lines.toList.map(parse)

val moves = input.foldLeft(Map[Int, (Target, Target)]())((acc, v) => {
  v match {
    case Move(b, l, h) => acc + (b -> (l, h))
    case _ => acc
  }
})

type Assignments = Map[Int, Int]
type Outputs = Map[Int, Int]

case class State(assn: Assignments, outs: Outputs)

def move(b: Int, x: Int, y: Int, state: State): State = {
  val List(v1, v2) = List(x, y).sorted
  if (v1 == 17 && v2 == 61) println(s"bot=$b")
  val (l, h) = moves(b)
  val s1 = State(state.assn - b, state.outs)
  val s2 = l match {
    case Output(o) => State(s1.assn, s1.outs + (o -> v1))
    case Bot(bot) => assign(v1, bot, s1)
  }
  h match {
    case Output(o) => State(s2.assn, s2.outs + (o -> v2))
    case Bot(bot) => assign(v2, bot, s2)
  }
}

def assign(x: Int, b: Int, s: State): State = {
  if (s.assn.contains(b)) move(b, x, s.assn(b), s)
  else State(s.assn + (b -> x), s.outs)
}

val emptyState = State(Map[Int, Int](), Map[Int, Int]())
val sol = input.foldLeft(emptyState)((acc, v) => {
  v match {
    case Assn(v, b) => assign(v, b, acc)
    case _ => acc
  }
})

val v0 = sol.outs(0)
val v1 = sol.outs(1)
val v2 = sol.outs(2)
println(s"prod=${v0 * v1 * v2}")
