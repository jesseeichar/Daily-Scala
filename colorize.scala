
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
      val blocks = line.lines.next match {
        case block.Contains (before, b, after ) => splitInBlocks(before) ::: (Some(block),b) :: splitInBlocks(after)
        case block.Starts (b, after) => (Some(block),b) :: splitInBlocks(after)
        case block.Ends (before, b) => splitInBlocks(before) ::: (Some(block),b) :: Nil
      }
      if(line.lines.next.size < line.size) blocks ::: (None, "") :: Nil else blocks
    }
  }
}

import scala.util.matching.Regex
def applyStyle(line:String, word:Regex, style:String) = {
  
  val spans = (List((false,"")) /: line) {
    case ((true,  seg) :: rest, char) if(seg endsWith "</span>") => (false,char.toString) :: (true,  seg) :: rest
    case ((true,  seg) :: rest, char) => (true,  seg+char) :: rest
    case ((false,  seg) :: rest, '<') => (false,"<") :: (false,  seg) :: rest
    case ((false,  seg) :: rest, '>') if(seg startsWith "<span") => (true,  seg+'>') :: rest
    case ((false,  seg) :: (b,prev) :: rest, '>') => (false,  prev+seg+'>') :: rest
    case ((false,  seg) :: rest, char) => (false,  seg+char) :: rest

    case (Nil, char) => throw new RuntimeException(char.toString)
  }

// println("line: "+line+" -- "+spans.mkString("{",",","}"))
  val result = spans.reverse map { 
    case (false,b) => word replaceAllIn (b,  """$1<span class="%s">$2</span>$3""".format(style))
    case (true,b) => b
    
  }

  result.mkString("")
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

def processCode(withIndices:Boolean, lines:Iterator[String])={
  val (styled, endBlock) = ((List[String](),None:Option[Block]) /: lines){
    case ((all, None), line) => {
      val (styled, endBlock) = process(line)
      (styled :: all, endBlock)
    }
    case ((all,Some(b)),line) => {
      val (styled, endBlock) = process (line, b)
      (styled :: all, endBlock)
    }
  }

  if(withIndices) {
    val table = styled.reverse.zipWithIndex.map {case (l,i) => """<li class="codelist %s">%s</li>""".format(if(i % 2 == 0) "alt" else "",l)}
    """<div class="codelist"><ol class="codelist">"""+table.mkString("")+"</ol></div>"
  } else {
    styled.reverse.mkString("")
  }
}

val source=scala.io.Source.fromFile(args(0))

val data = source.getLines.mkString("")

val startWithCode = data.trim startsWith "<code>"
val splitData = (data split "<code>|</code>")

val zipped = splitData.zipWithIndex map { case (line, index) => (line, 1 == (if (startWithCode) (index+1) % 2 else index %2)) }

val processed = zipped map {
  case (code, true) => {
    def newLine(f:(String)=>Boolean) = if (f("\n")) "\n" else ""
    "<code>" + newLine(code.startsWith _) + processCode (code.lines.toList.length>1, code.trim.linesWithSeparators) + newLine(code.endsWith _) + "</code>"
  }
  case (noncode, false) => noncode
}

//println("===================================================")
println("""<html>
  <head>
    <link rel="STYLESHEET" type="text/css" href="code.css">
  </head>
  <body>""")
println (processed.mkString(""))
println("</body></html>")
/*val (_, processed) = ((startWithCode,"") /: splitData) {
  case ((true,styled), code) => (false, (styled + "<code>%s</code>") format processCode(code.lines) )
    case ((false,styled),noncode) => (true,styled+noncode)
}*/

//println(processed)
