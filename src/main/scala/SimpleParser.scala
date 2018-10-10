import scala.util.parsing.combinator._

// Chaliana Rolón Ojeda #16-6780
// PL Take Home #1 Submission
// Credit to Luis F. Domenech and Luis R. Estrada
// References:
// https://dzone.com/articles/getting-started-with-scala-parser-combinators
// http://matt.might.net/articles/grammars-bnf-ebnf/
// https://docs.scala-lang.org/getting-started-intellij-track/building-a-scala-project-with-intellij-and-sbt.html
// And Others..

case class ID(str: String)
case class DELIMETER(str: String)
case class OPERATOR(str: String)
case class PRIM(str: String)
case class INTEGER(str: String)
case class BOOL(boolean: String)
case class BINOP(op: OPERATOR)
case class UNOP(op: OPERATOR)
case class SIGN(op: OPERATOR)
case class DEF(id: Any, exp: Any)
case class EXP(cont1: Any, cont2: Any)
case class FACTOR(content: Any)
case class TERM(t1: Any, t2: Any)
case class PROPEXPLIST(exp: Any, list: List[Any])
case class PROPIDLIST(id: ID, list: List[ID])
case class IDLIST(list: Any)
case class EXPLIST(list: Any)

class SimpleParser extends RegexParsers {

  override def skipWhitespace = true
  override val whiteSpace = "[\\t\\s\\n]+".r

  //////////////////// TOKENS /////////////////////
  val number = "[0-9]+".r
  val boolean = "true".r | "false".r | "null".r
  val charac = "[a-zA-Z?_]".r
  val opera = "!=" | "<" | ">" | "<=" | ">=" | ":=" |"+" | "-" | "*" | "/" | "=" | "&" | "|"
  val keyWords = "if" | "then" | "else" | "let" | "in" | "map" | "to"

  def delimiter: Parser[DELIMETER] = ("(" | ")" | "[" | "]" | "," | ";") ^^ { deli => DELIMETER(deli)}
  def operator: Parser[OPERATOR] = opera ^^ { opera => OPERATOR(opera)}
  def prim: Parser[PRIM] = ("number?" | "function?" | "list?" | "empty?" | "cons?" | "null?" | "cons" | "first" | "rest" | "arity") ^^ {prim => PRIM(prim)}
  def reserved: Parser[String] = keyWords ^^ {_.toString}
  def word: Parser[String] = charac ^^ {_.toString}
  def digit: Parser[String] = number ^^ {_.toString}
  def int: Parser[INTEGER] = "[0-9]+".r ^^ (int => INTEGER(int))
  def bool: Parser[BOOL] = boolean ^^ {case b => BOOL(b)}

  def sign: Parser[SIGN] = opera ^^ {
    case "+" => SIGN(OPERATOR("+"))
    case "-" => SIGN(OPERATOR("-"))
  }

  def unop: Parser[UNOP] = ("+" | "-" | "~") ^^ {
    case opera => UNOP(OPERATOR(opera))
  }

  def binop: Parser[BINOP] = ("<=" | ">=" | "!=" | "&" | "|" | "+" | "-" | "*" | "/" | "=" | "<" | ">") ^^ {
    case opera => BINOP(OPERATOR(opera))
  }

  def idL: Parser[IDLIST] = propid ^^ {
    case list => IDLIST(list)
  }

  def expL: Parser[EXPLIST] = propexp ^^ {
    case list => EXPLIST(list)
  }

  def propexp: Parser[Any] = (exp ~ rep("," ~> exp)) ^^ {
    case exp ~ list => PROPEXPLIST(exp, list)
    case exp ~ Nil => exp
  }
  def propid: Parser[Any] = (id ~ rep("," ~> id)) ^^ {
    case id ~ list => PROPIDLIST(id, list)
    case id ~ Nil => id
  }

  def Def: Parser[Any] = (id ~ ":=" ~ exp ~ ";") ^^ {
    case id ~ exp => DEF(id, exp)
  }

  // Bypass the reserved words and boolean tokens as ids
  val ignore = not(reserved | bool)
                        // P1           // P2
  def id: Parser[ID] = (ignore ~> word ~ rep(ignore ~> word | digit)) ^^ {
    case w ~ k => ID(w+k)
    case w ~ Nil=> ID(w)
  }

  def factor: Parser[FACTOR] = (("(" ~ exp ~ ")") | prim | id) ^^ {
    case ID(id) => FACTOR(ID(id))
    case PRIM(prim) => FACTOR(PRIM(prim))
    case exp => FACTOR(exp)
  }

  def term: Parser[TERM] = ((unop ~ term) | (factor ~ "(" ~ ")") | (factor ~ opt("(" ~> expL <~ ")")) | int | bool) ^^ {
    case BOOL(bool) => TERM(BOOL(bool), None)
    case INTEGER(int) => TERM(INTEGER(int), None)
    case UNOP(op) ~ TERM(t1,t2) => TERM(UNOP(op), TERM(t1,t2))
    case FACTOR(f) ~ Some(c) => TERM(FACTOR(f), c)
    case FACTOR(f) ~ "(" ~ ")" => TERM(FACTOR(f), ())
    case FACTOR(f) ~ None => TERM(FACTOR(f), None)
  }

  def exp: Parser[Any] = ((term ~ opt(binop ~ exp)) |  ("if" ~ exp ~ "then" ~ exp ~ "else" ~ exp) | ("let" ~ rep1(Def) ~ "in" ~ exp) | ("map" ~ idL ~ "to" ~ exp)) ^^ {
    case "map" ~ idL ~ "to" ~ exp => " map " + idL + " to" + exp
    case "if" ~ e1 ~ "then" ~ e2 ~ "else" ~ e3 => " if " + e1 + " then " + e2 + " else " + e3
    case "let" ~ list ~ " in" ~ e => " let " + list + " in " + e
    case TERM(t1,t2) ~ Some(e) => TERM(t1, t2) + " " + e
    case TERM(t1,t2) ~ None => EXP(TERM(t1,t2), None)
    case e => e
  }

}

///////////////// TESTER ////////////////////////////
object Main {
  def main(args : Array[String]) {
    val parser = new SimpleParser

    val parseBoo = parser.parseAll(parser.exp, "let f := map n to if n = 0 then 1 else n * f(n-1); " +
      "in " +
      "let " +
      "f := map n,m,k to if (n <= 0 & n >= 0) | (n < 0 & n > 0 & n != 0) then number? else m / f(k + 1); " +
      "in " +
      "let x:=3; " +
      "y:=4; " +
      "z:=cons?(function?(x * ~y), cons(-arity(x))); " +
      "in " +
      "let x:=3; " +
      "y:=4;" +
      "z:=g(); " +
      "in " +
      "(g(x,y,z))(null?(true),list?(false),first(null))")

    println(parseBoo)
  }

}
