package lexer

object Lexer {

  def generateTokenList(codeLines: Iterator[String]): List[(Int, List[Token])] = {
    var tokenLines: List[(Int, List[Token])] = List.empty
    var line: Int = 1

    while(codeLines.hasNext) {
      tokenLines = (line, parseLine(codeLines.next())) :: tokenLines
      line += 1
    }

    tokenLines
  }

  private def parseLine(codeLine: String): List[Token] = {
    var tokens: List[Token] = List.empty
    var currentToken: String = ""
    var pos: Int = 0

    while (pos < codeLine.length) {
      codeLine(pos) match {
        case ' ' | '\t' => // Scanning of next token complete -> parse token
          tokens = parseToken(currentToken) :: tokens
          currentToken = ""
        case '\n' =>
          tokens = parseToken(currentToken) :: tokens
          tokens = new EOLToken :: tokens
          currentToken = ""
        case '/' =>
          if (pos + 1 <= codeLine.length && codeLine(pos + 1) == '/') {
            pos = codeLine.length - 1
          }
          if (currentToken.nonEmpty) tokens = parseToken(currentToken) :: tokens
          tokens = new EOLToken :: tokens
          currentToken = ""
        case _ =>
          currentToken += codeLine(pos)
          pos += 1
      }
    }

    tokens
  }

  private def parseToken(token: String): Token = {
    null
  }

}
