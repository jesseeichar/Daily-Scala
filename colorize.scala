
val sep = """\A?([\s<>=+-:;/\\+,!\(\)\{\}'\|\&\^\[\] ]?)"""

val styles=Map(
  "val" -> "key",
  "var" -> "key",
  "def" -> "key",
  "trait" -> "key",
  "class" -> "key",
  "case" -> "key",
  "match" -> "key",
  "for" -> "key",
  "while" -> "key",
  "if" -> "key",
  "else" -> "key",
  "try" -> "key",
  "catch" -> "key",
  "finally" -> "key",
  "self" -> "key",
  "yield" -> "key",
  "object" -> "key",
  "Nil" -> "singleton",
  "Null" -> "singleton",
  "null" -> "singleton",
  "Unit" -> "singleton"
).map{case (k,v) => ((sep+"("+k+")"+sep).r,v)}

case class Block(start:String, end:String, styleClass:String){
  val Contains="""(\A.*)(%s.*%s)(.*\z)""".format(start,end).r
  val Starts="""^(.*)(%s.*\z)""".format(start).r
  val Ends = """^(.*%s)(.*\z)""".format(end).r

  override def toString = start+"..."+end
  
  private def index(f:String=>Int)(line:String) = f(line) match {
    case x if (x < 0) => None
    case x => Some(x)
  }
  def lastIndexOf = index(line => line.lastIndexOf(start)) _
  
  def indexOf = index(line => line.indexOf(start)) _
}

val blocks = List(Block("\"","\"", "quote"))

def splitInBlocks(line:String):List[(Option[Block],String)]={
  val valid = blocks.map {b=> (b, b indexOf line) }
  .filter { _._2.isDefined }
  .map {e => (e._1,e._2.get)}
  val sorted = valid sort { _._2 < _._2 }
 
  sorted match {
    case Nil => (None,line) :: Nil
    case (block,_) :: tail => {
      line match {
        case block.Contains (before, b, after ) => splitInBlocks(before) ::: (Some(block),b) :: splitInBlocks(after)
        case block.Starts (b, after) => (Some(block),b) :: splitInBlocks(after)
        case block.Ends (before, b) => splitInBlocks(before) ::: (Some(block),b) :: Nil 
      } 
    }
  }
}

import scala.util.matching.Regex
def applyStyle(line:String, word:Regex, style:String) = {
  word replaceAllIn (line,  """$1<span class="%s">$2</span>$3""".format(style))
}
def process(line:String):(String,Option[Block])={
  val blocks = splitInBlocks (line)
  val styled = blocks.map {
    case (Some(block),line) => ("""<span class="%s">%s</span>""".format (block.styleClass, line))
    case (None,line) => 
      (line /: styles) {
        case (line, (word,style)) if ((word findFirstIn line).isDefined) => applyStyle(line,word,style)
        case (line, _) => line
      }
  }
    
  val endBlock = blocks (blocks.length - 1) match { 
    case (Some(b), _) => line match {
      case b.Ends (_,_) => None
      case _ => Some(b)
    }
    case (None,_) => None
  }
  
  (styled mkString "", endBlock)
}

final def process(line:String, b:Block):(String,Option[Block])={
  line match {
    case b.Ends(r,l) =>{ 
      val (styled,endBlock) = process(l)
      (r+"" + styled, endBlock)
    }
    case r => (r, Some(b))
  }
}

def processCode(lines:Iterator[String])={
  val (styled, endBlock) = (("",None:Option[Block]) /: lines){
    case ((all, None), line) => {
      val (styled, endBlock) = process(line)
      (all+styled, endBlock)
    }
    case ((all,Some(b)),line) => {
      val (styled, endBlock) = process (line, b)
      (all+styled, endBlock)
    }
  }
  endBlock match {
    case Some(_) => styled + ""
    case None => styled
  }
}
  
val source=scala.io.Source.fromFile(args(0))

val data = source.getLines.mkString("")

val startWithCode = data.trim startsWith "<code>"
val splitData = (data split "<code>") flatMap (_ split "</code>")


val (_, processed) = ((startWithCode,"") /: splitData) {
  case ((true,styled), code) => (false, (styled + "<code>\n%s\n</code>") format processCode(code.lines) )
    case ((false,styled),noncode) => (true,styled+noncode)
}
  
println(processed)
