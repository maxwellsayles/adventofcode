val input: String = "10111011111001111"
//val diskSize: Int = 272
val diskSize: Int = 35651584

// Example input
// val input: String = "10000"
// val diskSize: Int = 20

def step(a: String): String = {
  val b = new StringBuilder()
  a.reverse.foreach(c => c match {
    case '0' => b += '1'
    case '1' => b += '0'
  })
  a + '0' + b.toString
}

def fillDisk(a: String): String =
  if (a.length >= diskSize) a.take(diskSize) else fillDisk(step(a))

def checksumStep(s: String): String = {
  val b = new StringBuilder()
  0.until(s.length).by(2).foreach(i => {
    val x = s(i)
    val y = s(i + 1)
    if (x == y) b += '1' else b += '0'
  })
  b.toString
}

def checksum(s: String): String =
  if (s.length % 2 == 1) s else checksum(checksumStep(s))

val solution = checksum(fillDisk(input))
println(solution)
