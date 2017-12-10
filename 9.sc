import scala.util.parsing.input.{NoPosition, Position, Reader}
import scala.util.parsing.combinator._
import scalaz.{-\/, \/, \/-}

val input = "{<garbage>,{<for your>},<garbage>,<{mind}>}"

{
  for (
    tokens <- StreamLexer(input);
    ast <- StreamParser(tokens)
  ) yield ast
} match {
  case -\/(err) => err
  case \/-(ast) =>
    println(s"Pt1: ${scoreAST(ast)}")
    println(s"Pt2: ${countGarbage(ast)}")
}

def scoreAST(ast: StreamAST, depth: Int = 1): Int = {
  ast match {
    case GroupASTNode(contents) =>
      depth + contents.map(subAST =>
        scoreAST(ast = subAST, depth = depth + 1)
      ).sum
    case _ => 0
  }
}

def countGarbage(ast: StreamAST): Int = {
  ast match {
    case GarbageASTNode(chars) =>
      """!.""".r
        .replaceAllIn(chars.slice(1, chars.length - 1), "")
        .length
    case GroupASTNode(contents) => contents.map(countGarbage).sum
    case _ => 0
  }
}

trait StreamCompilationError
case class StreamLexerError(msg: String) extends StreamCompilationError
case class StreamParserError(msg: String) extends StreamCompilationError

sealed trait StreamToken

case class GARBAGE(str: String) extends StreamToken
case class GROUP_START() extends StreamToken
case class GROUP_SEP() extends StreamToken
case class GROUP_END() extends StreamToken

object StreamLexer extends RegexParsers {
  override def skipWhitespace = false

  def apply(rawStream: String): \/[StreamLexerError, List[StreamToken]] =
    parse(tokenParser, rawStream) match {
      case NoSuccess(e, _) => -\/(StreamLexerError(msg = e))
      case Success(result, _) => \/-(result)
    }

  private def garbage: Parser[GARBAGE]        = """<(!.|[^!])*?>""".r ^^ { str => GARBAGE(str) }
  private def groupStart: Parser[GROUP_START] = "{" ^^ { _ => GROUP_START() }
  private def groupSep: Parser[GROUP_SEP]     = "," ^^ { _ => GROUP_SEP() }
  private def groupEnd: Parser[GROUP_END]     = "}" ^^ { _ => GROUP_END() }
  private def tokenParser: Parser[List[StreamToken]] =
    phrase(rep1(garbage | groupStart | groupSep | groupEnd))
}

sealed trait StreamAST
case class GarbageASTNode(str: String) extends StreamAST
case class GroupASTNode(contents: List[StreamAST]) extends StreamAST

object StreamParser extends Parsers {
  override type Elem = StreamToken

  class StreamTokenReader(tokens: Seq[StreamToken]) extends Reader[StreamToken] {
    override def first: StreamToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[StreamToken] = new StreamTokenReader(tokens.tail)
  }

  def apply(tokens: Seq[StreamToken]): \/[StreamParserError, StreamAST] = {
    phrase(group)(new StreamTokenReader(tokens)) match {
      case NoSuccess(msg, next) => -\/(StreamParserError(msg))
      case Success(result, next) => \/-(result)
    }
  }

  private def garbage: Parser[GarbageASTNode] =
    accept("garbage", { case GARBAGE(str) => GarbageASTNode(str) })

  private def groupOrGarbageSeq: Parser[List[StreamAST]] =
    repsep(garbage | group, GROUP_SEP())

  private def group: Parser[StreamAST] = (GROUP_START() ~ groupOrGarbageSeq ~ GROUP_END()) ^^
                                    { case id ~ contents ~ lit => GroupASTNode(contents) }
}
