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
          tokens = parseToken(currentToken, pos) ::: tokens
          currentToken = ""
          pos += 1
        case '\n' =>
          tokens = parseToken(currentToken, pos) ::: tokens
          tokens = new EOLToken :: tokens
          currentToken = ""
        case '/' =>
          if (pos + 1 <= codeLine.length && codeLine(pos + 1) == '/') {
            pos = codeLine.length - 1
          }
          if (currentToken.nonEmpty) tokens = parseToken(currentToken, pos) ::: tokens
          tokens = new EOLToken :: tokens
          currentToken = ""
        case _ =>
          currentToken += codeLine(pos)
          pos += 1
      }
    }

    tokens
  }

  private def parseToken(token: String, globalPos: Int): List[Token] = {
    var pos: Int = 0
    var tokens: List[Token] = List.empty

    while (pos < token.length) {

      token(pos) match {
        case '=' =>
          token(pos+1) match {
            case '=' =>
              pos += 1
              tokens = new EQToken :: tokens
            case _ =>
              tokens = new AssignmentToken :: tokens
          }

        case '|' =>
          token(pos+1) match {
            case '|' =>
              pos += 1
              tokens = new ORToken :: tokens
            case _ =>
              tokens =SyntaxErrorToken(globalPos + pos) :: tokens
          }

        case '&' =>
          token(pos+1) match {
            case '&' =>
              pos += 1
              tokens = new ANDToken :: tokens
            case _ =>
              tokens = SyntaxErrorToken(globalPos + pos) :: tokens
          }

        case '!' =>
          token(pos+1) match {
            case '=' =>
              pos += 1
              tokens = new NEToken :: tokens
            case _ =>
              tokens = new NOTToken :: tokens
          }

        case '<' =>
          token(pos+1) match {
            case '=' =>
              pos += 1
              tokens = new LEToken :: tokens
            case _ =>
              tokens = new LTToken :: tokens
          }

        case '>' =>
          token(pos+1) match {
            case '=' =>
              pos += 1
              tokens = new GEToken :: tokens
            case _ =>
              tokens = new GTToken :: tokens
          }

        case '(' =>
          tokens = new LeftParenToken :: tokens
          pos += 1
        case ')' =>
          tokens = new RightParenToken :: tokens
          pos += 1
        case '{' =>
          tokens = new BodyOpenToken :: tokens
          pos += 1
        case '}' =>
          tokens = new BodyCloseToken :: tokens
          pos += 1
        case ',' =>
          tokens = new SeparatorToken :: tokens
          pos += 1
        case ';' =>
          tokens = new TerminatorToken :: tokens
          pos += 1
        case '+' =>
          tokens = new AddToken :: tokens
          pos += 1
        case '-' =>
          tokens = new SubToken :: tokens
          pos += 1
        case '*' =>
          tokens = new MulToken :: tokens
          pos += 1
        case '/' =>
          tokens = new DivToken :: tokens
          pos += 1
        case '%' =>
          tokens = new ModToken :: tokens
          pos += 1

        case _ =>
          var subString: String = ""
          var foundToken: Token = SyntaxErrorToken(pos)

          while (pos < token.length && foundToken.isInstanceOf[SyntaxErrorToken]) {
            subString += token(pos)
            subString match {
              case "break" =>
                foundToken = new BreakToken
              case "while" =>
                foundToken = new WhileToken
              case "do" =>
                foundToken = new DoToken
              case "if" =>
                foundToken = new IfToken
              case "else" =>
                foundToken = new ElseToken
              case "return" =>
                foundToken = new ReturnToken
              case "void" =>
                foundToken = new VoidToken
              case "local" =>
                foundToken = new LocalToken
              case "global" =>
                foundToken = new GlobalToken
              case "true" =>
                foundToken = BooleanToken(true)
              case "false" =>
                foundToken = BooleanToken(false)
              case _ =>
                pos += 1
            }
          }

          if (foundToken.isInstanceOf[SyntaxErrorToken]) {
            subString.head match { // Try to match the next Integer literal
              case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '+' =>
                var integerLiteral: String = "" + subString.head
                while (subString(pos).isDigit) { // Add chars to intLiteral as long as they're digits
                  integerLiteral += subString(pos)
                  pos += 1
                }
                foundToken = IntegerToken(integerLiteral.toInt)
              case _ if subString.head.isLetter || subString.head == '_' =>
                var identifier: String = "" + subString.head
                while (subString(pos).isLetter || subString(pos) == '_'){
                  identifier += subString(pos)
                  pos += 1
                }
                foundToken = IdentifierToken(identifier)
            }
          }

          tokens = foundToken :: tokens
      }

      pos += 1
    }

    tokens
  }

}
