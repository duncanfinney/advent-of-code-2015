import fastparse.WhitespaceApi

object ParserTest extends App {

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  sealed trait Expr
  case class Let(identifier: String, value: String) extends Expr

  val identifier : Parser[String] =  P(CharIn('a' to 'z').rep.!).map(_.toString)
  val expression : Parser[String] = P(CharIn('a' to 'z').rep.!).map(_.toString)
  val assignment = P("let" ~ identifier ~ "=" ~ expression ~ End).map{
    case (ident, exp) => Let(ident.toString, exp.toString)
  }

  println(assignment.parse("let a = z"))

}