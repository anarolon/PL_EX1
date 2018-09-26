import scala.util.parsing.combinator._


// """[a-z]""".r is a regular expression
// ^^ {..} "a parser combinator for function application", If the parser combination
// on the left succeeds, the function on the right is executed (_.toString)
// Add custom Scala code to toperate on the parser results

case class ID(word: String)
case class SIGN(sign: String)
case class BOOL(boolean: Boolean)
case class BINOP(op: String)
case class UNOP(op: String)

class SimpleParser extends RegexParsers {

  //////////////////// TOKENS /////////////////////
  val number = "[0-9]+".r
  val boolean = "true".r | "false".r
  val charac = "[a-zA-Z?_]+".r
  val deli = "(" | ")" | "[" | "]" | "," | ";"
  val opera = "+" | "-" | "*" | "/" | "=" | "!=" | "<" | ">" | "<=" | ">=" | "&" | "|" | ":="

  def delimiter: Parser[String] = deli ^^ {
    _.toString
  }

  /**
  def operator: Parser[String] = opera ^^ {
    _.toString
  }
  */
  def word: Parser[String] = charac ^^ {_.toString}
  def digit: Parser[String] = number ^^ {_.toString}
  def inte: Parser[Int] = digit ^^ (_.toInt)



  def bool: Parser[BOOL] = boolean ^^ {
    _.toBoolean
  } ^^ {
    case b => BOOL(b)
  }

  def sign: Parser[SIGN] = opera ^^ {
    case "+" => SIGN("+")
    case "-" => SIGN("-")
  }

  def unop: Parser[UNOP] = (sign | opera) ^^ {
    case sign => UNOP(sign.toString)
    case "~" => UNOP("~")
  }

  def binop: Parser[BINOP] = (opera | sign) ^^ {
    case sign => BINOP(sign.toString)
    case "*" => BINOP("*")
    case "/" => BINOP("/")
    case "=" => BINOP("=")
    case "!=" => BINOP("!=")
    case "<" => BINOP("<")
    case ">" => BINOP(">")
    case "=>" => BINOP("=>")
    case ">=" => BINOP(">=")
    case "&" => BINOP("&")
    case "|" => BINOP("|")
  }

                        // P1           // P2
  def id: Parser[ID] = ((word ~ digit) | word) ^^ {
    case w ~ k => ID(w+k.toString)
    case w => ID(w.toString)
  }
}

///////////////// TESTER ////////////////////////////
object Main {
  def main(args : Array[String]) {
    val parser = new SimpleParser
    val boo = parser.parseAll(parser.bool, "true")
    val idTEST = parser.parseAll(parser.id, "c")
    val signTEST = parser.parseAll(parser.sign, "-")
    val binopTEST = parser.parseAll(parser.binop, "&")
    val unopTEST = parser.parseAll(parser.unop, "+")

    println(boo)
    println(idTEST)
    println(signTEST)
    println(binopTEST)
    println(unopTEST)
  }

}
