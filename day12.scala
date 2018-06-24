val input = io.Source.fromFile("day12.txt").mkString.lines.toArray

case class State(
  regs: Map[Char, Int],
  ip: Int
)

def step(program: Array[String], state: State): State = {
  val cpy = """cpy (\d+|\w) (\w)""".r
  val inc = """inc (\w)""".r
  val dec = """dec (\w)""".r
  val jnz = """jnz (\w) (-?\d+)""".r
  program(state.ip) match {
    case cpy(x, y) => {
      val v = state.regs.get(x(0)) match {
        case None => x.toInt
        case Some(v) => v
      }
      State(state.regs + (y(0) -> v), state.ip + 1)
    }

    case inc(r) => {
      val v = state.regs(r(0)) + 1
      State(state.regs + (r(0) -> v), state.ip + 1)
    }

    case dec(r) => {
      val v = state.regs(r(0)) - 1
      State(state.regs + (r(0) -> v), state.ip + 1)
    }

    case jnz(x, y) => {
      val v = state.regs.get(x(0)) match {
        case None => x.toInt
        case Some(v) => v
      }
      if (v != 0)
        State(state.regs, state.ip + y.toInt)
      else
        State(state.regs, state.ip + 1)
    }

    case _ => throw new RuntimeException("Unrecognized instruction: " + program(state.ip))
  }
}

val initState = State(Map('a' -> 0, 'b' -> 0, 'c' -> 0, 'd' -> 0), 0)

def solve(state: State): State = {
  if (state.ip >= input.size)
    state
  else
    solve(step(input, state))
}

println(solve(initState).regs('a'))

