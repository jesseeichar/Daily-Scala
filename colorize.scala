def logStyle(msg: =>String) = () //Console.err.println("[logStyle] "+msg)
def logSplitBlocks(msg: =>String) = () //Console.err.println("[splitBlocks] "+msg)
def logProcessCode(msg: =>String) = () //Console.err.println("[processCode] "+msg)
def logProcess(msg: =>String) = () //Console.err.println("[process] "+msg)

val sep = """(\A|[\s<>=+-:;/\\+,!\(\)\{\}'\|\&\^\[\] ]|\z)"""

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
  "override" -> "key",
  "if" -> "key",
  "else" -> "key",
  "try" -> "key",
  "catch" -> "key",
  "finally" -> "key",
  "self" -> "key",
  "yield" -> "key",
  "private" -> "key",
  "protected" -> "key",
  "public" -> "key",
  "object" -> "key",
  "import" -> "key",
  "Nil" -> "singleton",
  "Null" -> "singleton",
  "null" -> "singleton",
  "Unit" -> "singleton",
  "false" -> "singleton",
  "true" -> "singleton"
).map{case (k,v) => ((sep+"("+k+")"+sep).r,v)}

case class Block(start:String, end:String, styleClass:String){
  val Contains="""(\A.*)(%s.*%s)(.*\z)""".format(start,end).r
  val Starts="""^(.*)(%s.*\z)""".format(start).r
  val Ends = """^(.*%s)(.*\z)""".format(end).r

  override def toString = start+"..."+end

  def indexOf(line:String) =  line match {
    case Starts(_,s) => Some(line.indexOf(s))
    case _ => None
  }
}


val blocks = List(Block("&lt;","&gt;", "xml"),
                  Block("\"\"\"","\"\"\"", "string"),
                  Block("\"","\"", "string"),
                  Block("""/\*\*""", """\*/""", "javadoc"),
                  Block("""//""", """\z""", "comment"),
                  Block("""/\*""", """\*/""", "comment"),
                  Block("'","'", "char")
                )

def splitInBlocks(line:String):List[(Option[Block],String)]={
  val valid = blocks.map {b=> (b, b indexOf line) }
  .filter { _._2.isDefined }
  .map {e => (e._1,e._2.get)}
  val sorted = valid sort { _._2 <= _._2 }

  val split = sorted match {
    case Nil => (None,line) :: Nil
    case (block,_) :: tail => {
      val blocks = line.lines.next match {
        case block.Contains (before, b, after ) => splitInBlocks(before) ::: (Some(block),b) :: splitInBlocks(after)
        case block.Starts (before, b) => splitInBlocks(before) ::: (Some(block),b) :: Nil
        case block.Ends (b, after) => (Some(block),b) :: splitInBlocks(after)
      }
      if(line.lines.next.size < line.size) blocks ::: (None, "") :: Nil else blocks
    }
  }

  logSplitBlocks(line+" --> "+split)
  split
}

import scala.util.matching.Regex
def applyStyle(line:String, word:Regex, style:String) = {
  
  val spans = (List((false,"")) /: line) {
    case (l @ (true,  seg) :: rest, char) if(seg endsWith "</span>") => {logStyle("1: "+l); (false,char.toString) :: (true,  seg) :: rest}
    case (l @ (true,  seg) :: rest, char) => {logStyle("2: "+l); (true,  seg+char) :: rest}
    case (l @ (false,  seg) :: rest, '<') => {logStyle("3: "+l); ;(false,"<") :: (false,  seg) :: rest}
    case (l @ (false,  seg) :: rest, '>') if(seg startsWith "<span") => {logStyle("4: "+l); (true,  seg+'>') :: rest}
    case (l @ (false,  seg) :: (b,prev) :: rest, '>') if(seg startsWith "<")=> {logStyle("5: "+l); (false,  prev+seg+'>') :: rest}
    case (l @ (false,  seg) :: rest, char) => {logStyle("6: "+l); (false,  seg+char) :: rest}

    case (Nil, char) => throw new RuntimeException(char.toString)
  }

logStyle("line: "+line+" -- "+spans.mkString("{",",","}"))
  val result = spans.reverse map { 
    case (false,b) => {logStyle("replacing "+b+" based on "+word); word replaceAllIn (b,  """$1<span class="%s">$2</span>$3""".format(style))}
    case (true,b) => {logStyle("no replace"+b); b}
    
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

  val endBlock = blocks (blocks.length -1) match {
    case (Some(b), _) => line match {
      case b.Ends (a,b) if (b.isEmpty) => None
      case _ => Some(b)
    }
    case (None,_) => None
  }

  logProcess(blocks(blocks.length - 1).toString)

  logProcess("endBlock of line: "+endBlock)
  (styled mkString "", endBlock)
}

final def process(line:String, b:Block):(String,Option[Block])={
  val span = """<span class="%s">""".format(b.styleClass)

  line match {
    case b.Ends(r,l) =>{
      val (styled,endBlock) = process(l)
      (span + r + styled, endBlock)
    }
    case r => (span+r, Some(b))
  }
}

def processCode(withIndices:Boolean, lines:Iterator[String])={
  val nbSpaces = lines.toList.map( _.replace(" ", "&#8195;"))
  logProcessCode(nbSpaces.mkString("\n"))
  val (styled, endBlock) = ((List[String](),None:Option[Block]) /: nbSpaces){
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
    """<div class="codelist"><ol class="codelist">"""+"\n"+table.mkString("\n")+"\n</ol></div>"
  } else {
    styled.reverse.mkString("")
  }
}

val source=scala.io.Source.fromFile(args(0))

val data = source.getLines.mkString("").replaceAll("""<pre>|</pre>""","")

val startWithCode = data.trim startsWith "<code>"
val splitData = (data split "<code>|</code>")

val zipped = splitData.zipWithIndex map { case (line, index) => (line, 1 == (if (startWithCode) (index+1) % 2 else index %2)) }

val processed = zipped map {
  case (code, true) => {
    def newLine(f:(String)=>Boolean) = if (f("\n")) "\n" else ""
    "<code>" + newLine(code.startsWith _) + processCode (code.lines.toList.length>1, code.trim.lines) + newLine(code.endsWith _) + "</code>"
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
