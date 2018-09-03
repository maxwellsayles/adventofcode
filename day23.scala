val program = io.Source.fromFile("day23.txt").mkString.lines.toArray

case class State(
  regs: Map[Char, Int],
  ip: Int
)

def step(state: State): State = {
  val cpy = """cpy (-?\d+|\w) (\w+)""".r
  val inc = """inc (\w+)""".r
  val dec = """dec (\w+)""".r
  val jnzc = """jnz (\w+) (-?\d+)""".r
  val jnzr = """jnz (\w+) (\w)""".r
  val tglc = """tgl (-?\d+)""".r
  val tglr = """tgl (\w)""".r

  def tgl(d: Int) {
    val loc = state.ip + d
    if (loc >= 0 && loc < program.size) {
      program(loc) match {
        case cpy(x, y) => {
          program.update(loc, s"jnz ${x} ${y}")
        }

        case inc(x) => {
          program.update(loc, s"dec ${x}")
        }

        case dec(x) => {
          program.update(loc, s"inc ${x}")
        }

        case jnzc(x, y) => {
          program.update(loc, s"cpy ${x} ${y}")
        }

        case jnzr(x, y) => {
          program.update(loc, s"cpy ${x} ${y}")
        }

        case tglc(x) => {
          program.update(loc, s"inc ${x}")
        }

        case tglr(x) => {
          program.update(loc, s"inc ${x}")
        }

        case _ => {
          throw new RuntimeException("Toggling unrecognized instruction: " + program(loc))
        }
      }
    }
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

    case tglc(x) => {
      tgl(x.toInt)
      State(state.regs, state.ip + 1)
    }

    case tglr(x) => {
      tgl(state.regs(x(0)))
      State(state.regs, state.ip + 1)
    }

    case _ => {
      throw new RuntimeException("Unrecognized instruction: " + program(state.ip))
    }
  }
}

val initState = State(Map('a' -> 7, 'b' -> 0, 'c' -> 0, 'd' -> 0), 0)

def solve(state: State): State = {
  if (state.ip >= program.size)
    state
  else
    solve(step(state))
}

println(solve(initState).regs('a'))

