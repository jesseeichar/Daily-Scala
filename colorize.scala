import scala.io._
import scala.util.parsing.combinator._
assert( 1 == args.length, "One argument is required, the file to colorize")

val source = File(args(0))
val dest = File(args(0)+".color")

object MyParsers extends RegexParsers {
  val codeStart:Parser[String] = """<code>"""
  val codeEnd:Parser[String] = """</code>"""
  val basic:Parser[String] = """+""".r
  val code = codeStart~basic~codeEnd
  val doc = rep(basic)~rep(code)~rep(basic)

}

import MyParsers._
println(parseAll(doc, source.reader))
