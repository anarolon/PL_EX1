import scala.util.parsing.combinator._


// """[a-z]""".r is a regular expression
// ^^ {..} "a parser combinator for function application", If the parser combination
// on the left succeeds, the function on the right is executed (_.toString)
// Add custom Scala code to toperate on the parser results
class SimpleParser extends RegexParsers {

  //////////////////// TOKENS /////////////////////
  val number = "0|[0-9]+".r
  def delimiter: Parser[Any] = "(" | ")" | "[" | "]" | "," | ";"
  def operator: Parser[Any] = "+" | "-" | "*" | "/" | "=" | "!=" | "<" | ">" | "<=" | ">=" | "&" | "|" | ":="
  def word: Parser[String]  ="""[a-zA-Z]*[\s]*[a-zA-Z0-9]*""".r ^^ {_.toString}

  //////////////////// GRAMMAR/////////////////////
  def expr: Parser[Int] = (number ^^ { _.toInt }) ~ opt(operator ~ expr ) ^^ {
    case a ~ None => a              // Single Int Value
    case a ~ Some("*" ~ b) => a * b // Multiplication
    case a ~ Some("/" ~ b) => a / b // Division
    case a ~ Some("+" ~ b) => a + b // Addition
    case a ~ Some("-" ~ b) => a - b // Substraction
  }
}
/////////////////////////////////////////////////////

///////////////// TESTER ////////////////////////////
object Main {
  def main(args : Array[String]) {
    val parser = new SimpleParser
    val result = parser.parseAll(parser.expr, "2*8+21/7")
    val word = parser.parseAll(parser.word, "99")
    val deli = parser.parseAll(parser.delimiter, "(")
    val ope = parser.parseAll(parser.operator, "+")
    println(word.get)
    println(deli.get)
    println(ope.get)
  }

}
