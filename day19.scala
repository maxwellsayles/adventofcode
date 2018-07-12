val input = 3017957

def solve(i: Int, j: Int, k: Int): Int = {
  if (i == j) i
  else {
    val d = (i - j) / k;
    if (d % 2 == 0) solve(i + 2 * k, j, 2 * k)
    else solve(i, j - k, 2 * k)
  }
}

println(solve(1, input, 1))
