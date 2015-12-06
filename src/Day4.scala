import java.math.BigInteger
import java.security.MessageDigest

object Day4 extends App {

  def md5Hash(text: String) : String = java.security.MessageDigest.getInstance("MD5").digest(text.getBytes("UTF-8")).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}

  def answer(secretKey: String) : Int = {
    for (n <- Stream.from(0)) {
      val hash = md5Hash(secretKey + n)
      if (hash.startsWith("000000")) { //padded zeros removed
        return n
      }
    }
    throw new Error("Wow...")
  }


//  assert(answer("abcdef") == 609043)
//  assert(answer("pqrstuv") == 1048970)
  println(answer("bgvyzdsv"))


}