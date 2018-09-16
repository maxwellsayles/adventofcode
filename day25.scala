val program = io.Source.fromFile("day25.txt").mkString.lines.toArray
var output: String = ""

case class State(
  regs: Map[Char, Int],
  ip: Int
)

def multiply(state: State): State = {
  val newD = state.regs('d') + 282 * 9
  val regs = state.regs + ('d' -> newD) + ('b' -> 0) + ('c' -> 0)
  State(regs, state.ip + 7)
}

def step(state: State): State = {
  val cpy = """cpy (-?\d+|\w) (\w+)""".r
  val inc = """inc (\w+)""".r
  val dec = """dec (\w+)""".r
  val jnzc = """jnz (\w+) (-?\d+)""".r
  val jnzr = """jnz (\w+) (\w)""".r

  if (state.ip == 1) {
    // Special case some statements that are the equivalent of a multiply.
    return multiply(state)
  }

  program(state.ip) match {
    case cpy(x, y) => {
      val v = state.regs.get(x(0)) match {
        case None => x.toInt
        case Some(v) => v
      }
      if (state.regs.contains(y(0))) {
        State(state.regs + (y(0) -> v), state.ip + 1)
      } else {
        State(state.regs, state.ip + 1)
      }
    }

    case inc(reg) => {
      val r = reg(0)
      if (state.regs.contains(r)) {
        val v = state.regs(r) + 1
        State(state.regs + (r -> v), state.ip + 1)
      } else {
        State(state.regs, state.ip + 1)
      }
    }

    case dec(reg) => {
      val r = reg(0)
      if (state.regs.contains(r)) {
        val v = state.regs(r) - 1
        State(state.regs + (r -> v), state.ip + 1)
      } else {
        State(state.regs, state.ip + 1)
      }
    }

    case jnzc(x, y) => {
      val v = state.regs.get(x(0)) match {
        case None => x.toInt
        case Some(v) => v
      }
      if (v != 0)
        State(state.regs, state.ip + y.toInt)
      else
        State(state.regs, state.ip + 1)
    }

    case jnzr(x, y) => {
      val v = state.regs.get(x(0)) match {
        case None => x.toInt
        case Some(v) => v
      }
      if (v != 0)
        State(state.regs, state.ip + state.regs(y(0)))
      else
        State(state.regs, state.ip + 1)
    }

    case "out b" => {
      output += state.regs('b')
      State(state.regs, state.ip + 1)
    }

    case _ => {
      throw new RuntimeException("Unrecognized instruction: " + program(state.ip))
    }
  }
}

val initState = State(Map('a' -> 0, 'b' -> 0, 'c' -> 0, 'd' -> 0), 0)

def solveRec(state: State): String = {
  if (state.ip >= program.size - 1)
    output
  else
    solveRec(step(state))
}

def solve(state: State): Int = {
  output = ""
  solveRec(state)
  if (output.length != 12) {
    println("Unexpected length: " + output.length)
    return -1
  }
  println(output)
  if (output == "010101010101") {
    return state.regs('a')
  }
  solve(State(state.regs + ('a' -> (state.regs('a') + 1)), 0))
}

println(solve(initState))

