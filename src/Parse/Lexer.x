{
module Parse.Lexer (
  Parse.Lexer.lexer, 
  Token(..),
  )  
where
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "//".*				;
  let					{ \pos _ -> TokenLet pos }
  in					{ \pos _ -> TokenIn pos }
  $digit+				{ \pos s -> TokenInt (read s, pos) }
  [\=] { \pos _ -> TokenEq pos}
  [\+] { \pos _ -> TokenPlus pos}
  [\-] { \pos _ -> TokenMinus pos}
  [\*] { \pos _ -> TokenTimes pos}
  [\/] { \pos _ -> TokenDiv pos}
  [\(] { \pos _ -> TokenOB pos}
  [\)] { \pos _ -> TokenCB pos}

  $alpha [$alpha $digit \_ \']*		{ \pos s -> TokenVar (s, pos) }

{
-- Each action has type :: String -> Token

data Token
  = TokenLet AlexPosn
  | TokenIn AlexPosn
  | TokenVar (String, AlexPosn)
  | TokenInt (Int, AlexPosn)
  | TokenEq AlexPosn
  | TokenPlus AlexPosn
  | TokenMinus AlexPosn
  | TokenTimes AlexPosn
  | TokenDiv AlexPosn
  | TokenOB AlexPosn
  | TokenCB AlexPosn
  deriving (Eq)

instance Show Token where
  show (TokenLet pos) = show "let" ++ prettyAlexPosn pos
  show (TokenIn pos) = show "in" ++ prettyAlexPosn pos
  show (TokenVar (s, pos)) = show s ++ prettyAlexPosn pos
  show (TokenInt (n, pos)) = show n ++ prettyAlexPosn pos
  show (TokenEq pos) = show "=" ++ prettyAlexPosn pos
  show (TokenPlus pos) = show "+" ++ prettyAlexPosn pos
  show (TokenMinus pos) = show "-" ++ prettyAlexPosn pos
  show (TokenTimes pos) = show "*" ++ prettyAlexPosn pos
  show (TokenDiv pos) = show "/" ++ prettyAlexPosn pos
  show (TokenOB pos) = show "(" ++ prettyAlexPosn pos
  show (TokenCB pos) = show ")" ++ prettyAlexPosn pos

prettyAlexPosn (AlexPn offset line col) = 
  "at line " ++ show line ++ ", col " ++ show col

lexer = alexScanTokens
}