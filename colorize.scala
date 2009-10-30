
val file = args(args.length-1)
val pretty = args.find( _ == "-pretty").isDefined

def logStyle(msg: =>String) =() //Console.err.println("[logStyle] "+msg)
def logStyleFine(msg: =>String) = () //Console.err.println("[logStyle] "+msg)
def logSplitBlocks(msg: =>String) =() //Console.err.println("[splitBlocks] "+msg)
def logProcessCode(msg: =>String) = () //Console.err.println("[processCode] "+msg)
def logProcess(msg: =>String) = () //Console.err.println("[process] "+msg)
def logMain(msg: =>String) = () //Console.err.println("[main] "+msg)
def qlog(msg: =>String) = Console.err.println("[quicklog] "+msg)

import scala.util.matching.Regex

val sepCharacters = """[\s<>=+-:;/\\+,!\(\)\{\}'\|\&\^\[\] ]"""
val sep = """(\A|"""+sepCharacters+"""|\z)"""

case class Block(start:String, end:String, styleClass:String, singleliner:Boolean, includeStartQuote:Boolean, includeEndQuote:Boolean){
  val Contains= (includeStartQuote, includeEndQuote) match {
    case (true,true) => """(\A.*)(%s.*?%s)(.*\z)""".format(start,end).r
    case (false,true) => """(\A.*%s)(.*?%s)(.*\z)""".format(start,end).r
    case (true,false) => """(\A.*)(%s.*?)(%s.*\z)""".format(start,end).r
    case (false,false) => """(\A.*%s)(.*?)(%s.*\z)""".format(start,end).r
  }
  val Starts=includeStartQuote match {
    case true => """^(.*)(%s.*?\z)""".format(start).r
    case false => """^(.*%s)(.*?\z)""".format(start).r
  }

  val Ends = includeEndQuote match {
    case true => """^(.*%s)(.*)""".format(end).r
    case false => """^(.*)(%s.*)""".format(end).r
  }

  override def toString = start+"..."+end

  def indexOf(line:String) =  line match {
      case Starts(_,s) => Some(line.indexOf(s))
      case _ => None
    }
}

case class Lang(name:String, blocks:List[Block], mapping:Iterable[(Regex,String)]) {
  override def toString = name
  def splitInBlocks(line:String):List[(Option[Block],String)]={
    val valid = blocks.map {b=> (b, b indexOf line) }.
                       filter { _._2.isDefined }.
                       map {e => (e._1,e._2.get)}
    val sorted = valid sort { _._2 <= _._2 }

    val firstBlock = sorted.firstOption

    val split = firstBlock match {
      case None => (None,line) :: Nil
      case Some((block,_)) => {
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
      case (false,b) => {logStyleFine("replacing "+b+" based on "+word); word replaceAllIn (b,  """$1<span class="%s">$2</span>$3""".format(style))}
      case (true,b) => {logStyleFine("no replace"+b); b}
      
    }

     result.mkString("")
  }
  
  def process(line:String):(String,Option[Block])={
    val blocks = splitInBlocks (line)
    val styled = blocks.map {
      case (Some(block),line) => ("""<span class="%s">%s</span>""".format (block.styleClass, line.replaceAll("<","&lt;").replaceAll(">","&gt;")))
        case (None,line) =>
          (line /: mapping) {
            case (line, (word,style)) if ((word findFirstIn line).isDefined) => applyStyle(line,word,style)
            case (line, (word, style)) => {logStyle("not styling: "+line+" with "+word);line}
          }
    }
    
    val endBlock = blocks (blocks.length -1) match {
      case (Some(b), _) if (!b.singleliner) => line match {
        case b.Ends (a,b) if (b.isEmpty) => None
        case _ => Some(b)
      }
      case (_,_) => None
    }
    
    logProcess(blocks(blocks.length - 1).toString)
    
    logProcess("endBlock of line: "+endBlock)
    (styled mkString "", endBlock)
  }
  
  final def process(line:String, b:Block):(String,Option[Block])={
    val span = """<span class="%s">""".format(b.styleClass)
    val endSpan = """</span>"""
    
    line match {
      case b.Ends(r,l) =>{
        val (styled,endBlock) = process(l)
        (span + r + styled+endSpan, endBlock)
      }
      case r => (span+r+endSpan, Some(b))
    }
  }
  
  def processCode(withIndices:Boolean, lines:List[String])(pretty:Boolean)={
    val nbSpaces = lines.map( _.replace(" ", "&#160;"))
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
    
    val itemSep = if(pretty) "\n    " else ""
    if(withIndices) {
      val table = styled.reverse.zipWithIndex.map {case (l,i) => """<li class="codelist %s">%s</li>""".format(if(i % 2 == 0) "alt" else "",l)}
      if(pretty)
        """<div class="codelist">
           |  <ol class="codelist">
           |    %s
           |  </ol>
           |</div>""".stripMargin.format(table.mkString(itemSep))
      else
        """<div class="codelist"><ol class="codelist">%s</ol></div>""" format table.mkString(itemSep)
    } else {
      styled.reverse.mkString("")
    }
  }
}

val Scala = Lang("Scala", 
                 List(Block("&lt;[^-]","&gt;", "xml",false, true,true),
                      Block("\"\"\"","\"\"\"", "string",false, true,true),
                      Block("\"","\"", "string",false, true,true),
                      Block("""/\*\*""", """\*/""", "javadoc",false, true,true),
                      Block("""//""", """\z""", "comment",false, true,true),
                      Block("""\[""", """\]""", "type-param",false, true,true),
                      Block("""/\*""", """\*/""", "comment",false, true,true),
                      Block("@", sepCharacters, "annotation",true, true,false),
                      Block("'","'", "char",false, true,true)
                    ),
                 Map("val" -> "key",
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
                     "extends" -> "key",
                     "abstract" -> "key",
                     "yield" -> "key",
                     "private" -> "key",
                     "protected" -> "key",
                     "public" -> "key",
                     "with" -> "key",
                     "lazy" -> "key",
                     "finally" -> "key",
                     "type" -> "key",
                     "new" -> "key",
                     "object" -> "key",
                     "throw" -> "key",
                     "default" -> "key",
                     "import" -> "key",
                     "Nil" -> "singleton",
                     "Null" -> "singleton",
                     "null" -> "singleton",
                     "Unit" -> "singleton",
//                     "_" -> "singleton",
//                     "_*" -> "singleton",
                     "unapply" -> "magic",
                     "unapplySeq" -> "magic",
                     "apply" -> "magic",
                     "update" -> "magic",
                     "false" -> "basicType",
                     "true" -> "basicType",
                     "Int" -> "basicType",
                     "String" -> "basicType",
                     "Boolean" -> "basicType",
                     "Char" -> "basicType",
                     "Long" -> "basicType",
                     "Byte" -> "basicType",
                     "Double" -> "basicType",
                     "Float" -> "basicType",
                     "scala>" -> "repl"
                   ).map{case (k,v) => ((sep+"("+k+")"+sep).r,v)}
               ) 
val Java = Lang("Java",
                List(Block("\"","\"", "string",false, true,true),
                     Block("""/\*\*""", """\*/""", "javadoc",false, true,true),
                     Block("""//""", """\z""", "comment",false, true,true),
                     Block("""/\*""", """\*/""", "comment",false, true,true),
                     Block("@", sepCharacters, "annotation",true, true,false),
                     Block("'","'", "char",false, true,true)
                   ),
                 Map("interface" -> "key",
                     "class" -> "key",
                     "case" -> "key",
                     "switch" -> "key",
                     "static" -> "key",
                     "new" -> "key",
                     "for" -> "key",
                     "while" -> "key",
                     "default" -> "key",
                     "if" -> "key",
                     "else" -> "key",
                     "try" -> "key",
                     "catch" -> "key",
                     "throw" -> "key",
                     "finally" -> "key",
                     "private" -> "key",
                     "protected" -> "key",
                     "public" -> "key",
                     "import" -> "key",
                     "int" -> "basicType",
                     "String" -> "basicType",
                     "boolean" -> "basicType",
                     "char" -> "basicType",
                     "long" -> "basicType",
                     "byte" -> "basicType",
                     "double" -> "basicType",
                     "float" -> "basicType",
                     "null" -> "singleton",
                     "false" -> "singleton",
                     "true" -> "singleton"
                   ).map{case (k,v) => ((sep+"("+k+")"+sep).r,v)}
               ) 



    val source=scala.io.Source.fromFile(file)
    val data = source.getLines.mkString("").replaceAll("""<pre>|</pre>""","")

    val CodeBlocks = """<code( class="(\w+)")?>([\s\S]*?)</code>""".r
    logMain (CodeBlocks.findAllIn(data).mkString("\n"))
    val (_, processed) = ( (0,data) /: (CodeBlocks findAllIn data.trim).matchData) (
      (result, matchData) => {
        val (offset,data) = result
        def withIndices(code:String):Boolean = code.lines.toList.length > 1
        def update(raw:String, lang:Lang) = {

          val code = raw//.replaceAll("<strong>|</strong>","")
          val processFunc = lang.processCode (withIndices (code), code.trim.lines.toList) _
          val updatedCode = processFunc(pretty)

          // check that the code is valid XML
          try{
            scala.xml.XML.loadString("<code>"+updatedCode+"</code>")
          }catch{
            case e => {
              val pretty = processFunc(true)
              println("<code>"+pretty+"</code>")
              scala.xml.XML.loadString(pretty)
            }
          }
          val patched = data.patch (matchData.start(3)+offset, updatedCode, code.length).mkString
          (offset + updatedCode.length - code.length, patched)
        }
        val updated = matchData match {
          case CodeBlocks(_,null,code) => update(code, Scala)
          case CodeBlocks(_,codeType,code) if(codeType.toLowerCase == "java") => update(code, Java)
          case CodeBlocks(_,codeType,code) => update(code, Scala)
        }
        logMain(matchData+" --> "+updated)
        updated
      })
    
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
    
