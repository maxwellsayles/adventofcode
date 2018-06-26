val md5Instance = java.security.MessageDigest.getInstance("MD5")

def md5(bs: Array[Byte]): Array[Byte] = {
  md5Instance.reset()
  md5Instance.update(bs)
  md5Instance.digest
}

def md5String(s: String): String =
  md5(s.getBytes).map(x => "%02x".format(x)).mkString

println(md5String("asdf"))
