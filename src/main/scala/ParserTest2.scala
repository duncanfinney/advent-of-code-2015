import fastparse.WhitespaceApi

object ParserTest2 extends App {

  /*
QUERY ::= 'SELECT' ('COUNT()' | (FIELD ( ',' FIELD)*))
'FROM' (NAME ('AS' ? NAME)? ('USING' NAME)?) ( ',' NAME ('AS' ? NAME)? ('USING' NAME)?)*
('WHERE' CONDITIONEXPR )?
('ORDER BY' ORDERBYEXPR)?
('LIMIT' POSINTEGER )?
FIELD ::= NAME | '(' QUERY ')'
CONDITIONEXPR ::= ANDEXPR | OREXPR | NOTEXPR | SIMPLEEXPR
ANDEXPR ::= 'AND' SIMPLEEXPR
OREXPR ::= 'OR' SIMPLEEXPR
NOTEXPR ::= 'NOT' SIMPLEEXPR
SIMPLEEXPR ::= '(' CONDITIONEXPR ')' | FIELDEXPR | SETEXPR
FIELDEXPR ::= NAME OPERATOR VALUE
SETEXPR ::= ( NAME ('includes' | 'excludes' | 'in' | 'not' 'in') '(' VALUE
 (',' VALUE)* ')' )
ORDERBYEXPR ::= NAME ('asc' | 'desc')? ('nulls' ('first'|'last'))? (',' NAME ('asc' |'desc')? ('nulls' ('first'|'last'))?)*
OPERATOR ::= '=' | '!=' | '<' | '<=' | '>' | '>=' | 'like'
LOGICALOPERATOR ::= 'AND' | 'OR ' | 'NOT'
VALUE ::= STRING_LITERAL | NUMBER | DATE | DATETIME | NULL | TRUE | FALSE | DATEFORMULA
DATEFORMULA ::= TODAY | TOMORROW | LAST_WEEK | THIS_WEEK | NEXT_WEEK | THIS_MONTH
 | LAST_MONTH | NEXT_MONTH | LAST_90_DAYS | NEXT_90_DAYS | LAST_N_DAYS ':' NUMBER
 | NEXT_N_DAYS ':' NUMBER
NAME ::= LETTER (NAMECHAR)*
LETTER ::= 'a'..'z' | 'A'..'Z'
NAMECHAR ::= LETTER | DIGIT | '_' | '.'
DATE ::= YEAR '-' MONTH '-' DAY
DATETIME ::= DATE 'T' HOUR ':' MINUTE ':' SECOND ('Z' | (('+' |'-') HOUR ':' MINUTE))
YEAR ::= DIGIT DIGIT DIGIT DIGIT
MONTH ::= '0' '1'..'9' | '1' ('0' | '1' | '2')
DAY ::= '0' '1'..'9' | '1'..'2' DIGIT | '3' ('0' | '1')
HOUR ::= '0'..'1' DIGIT | '2' '0'..'3'
MINUTE ::= '0'..'5' DIGIT
SECOND ::= '0'..'5' DIGIT | '60'
NULL ::= 'null'
TRUE ::= 'true'
FALSE ::= 'false'
NUMBER ::= '.' POSINTEGER | INTEGER '.' POSINTEGER
INTEGER ::= ('+' | '-')? POSINTEGER
POSINTEGER ::= DIGIT+
DIGIT ::= '0'..'9'
STRING LITERAL ::= "'" (ESC_CHAR | ~("'"|'\''"
ESC_CHAR ::= '\'n' | 'r' | 't' | 'b' | 'f' | '"' | "'" | '\'
WS ::= S+
S ::= ' ' | '' | '
   */

  case class Query(selectList: List[String], tableName: String)

  val WS = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }

  import fastparse.noApi._
  import WS._

  val field = P("f").map(_.toString)


  val letter = P(CharIn('A' to 'Z') | CharIn('a' to 'z'))
  val digit = P(CharIn('0' to '9'))

  val name = P(letter | digit | CharIn("_.")).rep
  val operator = P("=" | "!=" | "<" | "<=" | ">" | ">=" | "like")

  /**
    * Values
    */

  val posInteger = P(digit.rep(min = 1))
  val integer = P(("+" | "-").? ~ posInteger)

  val number = P(posInteger | integer ~ "." ~ posInteger)
  val value = P(number)


  /**
    * Expr
    */
  val fieldExpr = P(name ~ operator ~ value)

  val simpleExpr = P("(" ~ conditionExpr ~ ")" | fieldExpr )
  val andExpr = P("AND" ~ simpleExpr)
  val orExpr = P("OR" ~ simpleExpr)
  val notExpr = P("NOT" ~ simpleExpr)

  val conditionExpr: P[Any] = P(andExpr.! | orExpr.! | notExpr.! | simpleExpr.!)

  val query = P(
    "SELECT" ~
      ("COUNT()" | field).rep(0, ",").! ~
      "FROM" ~
      name.! ~
      ("WHERE" ~ conditionExpr).?
  )

  println(query.parse("SELECT COUNT() FROM theTable WHERE tableName=testing"))

}