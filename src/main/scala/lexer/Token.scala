package lexer

// Base Token class
class Token {}

// Tokens for describing program flow control
class FlowToken extends Token
case class IfToken() extends FlowToken
case class ElseToken() extends FlowToken
case class DoToken() extends FlowToken
case class WhileToken() extends FlowToken
case class BreakToken() extends FlowToken
case class ReturnToken() extends FlowToken

// Variable and function modifier tokens
class ModifierToken extends Token
case class VoidToken() extends ModifierToken
case class GlobalToken() extends ModifierToken
case class LocalToken() extends ModifierToken

// Tokens for boolean logic expression
class LogicToken extends Token
case class ANDToken() extends Token
case class ORToken() extends Token
case class NOTToken() extends Token

// Tokens for value comparisons
class CompareToken extends Token
case class EQToken() extends CompareToken
case class NEToken() extends CompareToken
case class GTToken() extends CompareToken
case class GEToken() extends CompareToken
case class LTToken() extends CompareToken
case class LEToken() extends CompareToken

// Tokens for arithmetic operations
class ArithmeticToken extends Token
case class AddToken() extends ArithmeticToken
case class SubToken() extends ArithmeticToken
case class MulToken() extends ArithmeticToken
case class DivToken() extends ArithmeticToken
case class ModToken() extends ArithmeticToken

// Tokens for representing literal values
class LiteralToken extends Token
case class BooleanToken(value: Boolean) extends LiteralToken
case class IntegerToken(value: Int) extends LiteralToken
case class CharacterToken(value: Char) extends LiteralToken

// Miscellaneous Tokens
case class IdentifierToken(identifier: String) extends Token
case class AssignmentToken() extends Token
case class SeparatorToken() extends Token
case class TerminatorToken() extends Token
case class BodyOpenToken() extends Token
case class BodyCloseToken() extends Token
case class LeftParenToken() extends Token  // Left Parentheses
case class RightParenToken() extends Token // Right Parentheses
case class AccessToken() extends Token // .  p.x = ...

case class EOLToken() extends Token
case class SyntaxErrorToken(pos: Int) extends Token
