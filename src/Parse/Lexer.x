{
module Parse.Lexer (
  lexer, 
  Token(..),
  Alex,
  alexError,
  runAlex,
  alexMonadScan
  )  
where
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
  $white+				;
  "//".*				;
  while { mkL LWhile}
  for {mkL LFor}
  to {mkL LTo}
  break {mkL LBreak}
  let	{ mkL LLet }
  in { mkL LIn }
  end {mkL LEnd}
  function {mkL LFunction}
  var {mkL LVar}
  type {mkL LType}
  array {mkL LArray}
  if {mkL LIf}
  then {mkL LThen}
  else {mkL LElse}
  do {mkL LDo}
  of {mkL LOf}
  nil {mkL LNil}
  $digit+				{ mkL LInt }
  $alpha [$alpha $digit \_ \']*		{ mkL LName}
  "\"" $printable* "\"" {mkL LString}
  "=" { mkL LEq}
  "<>" {mkL LNeq}
  "<" {mkL LLess}
  "<=" {mkL LLeq}
  ">" {mkL LGreater}
  ">=" {mkL LGeq}
  "." {mkL LDot}
  "," {mkL LComma}
  ":" {mkL LColon}
  ";" {mkL LSemicolon}
  "&" {mkL LAnd}
  "|" {mkL LPipe}
  ":=" {mkL LAssign}
  "+" { mkL LPlus }
  "-" { mkL LMinus }
  "*" { mkL LTimes }
  "/" { mkL LDiv }
  "(" { mkL LLParen }
  ")" { mkL LRParen }
  "{" { mkL LLBrace }
  "}" { mkL LRBrace }
  "[" { mkL LLBracket }
  "]" { mkL LRBracket }

{

data LexemeClass = LInt | LName | LString
  | LLet | LIn | LWhile | LFor | LBreak | LTo | LEnd | LFunction | LVar | LType | LArray 
  | LIf | LThen | LElse | LDo | LOf | LNil 
  | LComma | LDot | LColon | LSemicolon | LLParen | LRParen | LLBrace | LRBrace 
  | LLBracket | LRBracket | LPlus | LMinus | LTimes | LDiv
  | LEq | LNeq | LLess | LLeq | LGreater | LGeq
  | LAnd | LPipe | LAssign
  deriving (Eq, Show)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (pos, _, _, str) len = 
  case c of
    LInt -> return (TokenInt ((read tok), pos))
    LName -> return (TokenName (tok, pos))
    LString -> return (TokenString (tok, pos))
    LLet -> return (TokenLet pos)
    LIn -> return (TokenIn pos)
    LWhile -> return (TokenWhile pos)
    LFor -> return (TokenFor pos)
    LTo -> return (TokenTo pos)
    LBreak -> return (TokenBreak pos)
    LEnd -> return (TokenEnd pos)
    LFunction -> return (TokenFunction pos)
    LVar -> return (TokenVar pos)
    LType -> return (TokenType pos)
    LArray -> return (TokenArray pos)
    LIf -> return (TokenIf pos)
    LThen -> return (TokenThen pos)
    LElse -> return (TokenElse pos)
    LDo -> return (TokenDo pos)
    LOf -> return (TokenOf pos)
    LNil -> return (TokenNil pos)
    LLParen -> return (TokenLParen pos)
    LRParen -> return (TokenRParen pos)
    LLBrace -> return (TokenLBrace pos)
    LRBrace -> return (TokenRBrace pos)
    LLBracket -> return (TokenLBracket pos)
    LRBracket -> return (TokenRBracket pos)
    LDot -> return (TokenDot pos)
    LComma -> return (TokenComma pos)
    LColon -> return (TokenColon pos)
    LSemicolon -> return (TokenSemicolon pos)
    LEq -> return (TokenEq pos)
    LNeq -> return (TokenNeq pos)
    LPlus -> return (TokenPlus pos)
    LMinus -> return (TokenMinus pos)
    LTimes -> return (TokenTimes pos)
    LDiv -> return (TokenDiv pos)
    LLess -> return (TokenLess pos)
    LLeq -> return (TokenLeq pos)
    LGreater -> return (TokenGreater pos)
    LGeq -> return (TokenGeq pos)
    LAnd -> return (TokenAnd pos)
    LPipe -> return (TokenPipe pos)
    LAssign -> return (TokenAssign pos)
  where
    tok = take len str


alexEOF :: Alex Token
alexEOF = return Eof


data Token
  = TokenLet AlexPosn
  | TokenIn AlexPosn
  | TokenWhile AlexPosn
  | TokenFor AlexPosn
  | TokenTo AlexPosn
  | TokenBreak AlexPosn
  | TokenEnd AlexPosn
  | TokenFunction AlexPosn
  | TokenVar AlexPosn
  | TokenType AlexPosn
  | TokenArray AlexPosn
  | TokenIf AlexPosn
  | TokenThen AlexPosn
  | TokenElse AlexPosn
  | TokenDo AlexPosn
  | TokenOf AlexPosn
  | TokenNil AlexPosn
  | TokenName (String, AlexPosn)
  | TokenString (String, AlexPosn)
  | TokenInt (Int, AlexPosn)
  | TokenEq AlexPosn
  | TokenPlus AlexPosn
  | TokenMinus AlexPosn
  | TokenTimes AlexPosn
  | TokenDiv AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenComma AlexPosn
  | TokenColon AlexPosn
  | TokenSemicolon AlexPosn
  | TokenLBracket AlexPosn
  | TokenRBracket AlexPosn
  | TokenLBrace AlexPosn
  | TokenRBrace AlexPosn
  | TokenDot AlexPosn
  | TokenNeq AlexPosn
  | TokenLess AlexPosn
  | TokenLeq AlexPosn
  | TokenGreater AlexPosn
  | TokenGeq AlexPosn
  | TokenAnd AlexPosn
  | TokenPipe AlexPosn
  | TokenAssign AlexPosn
  | Eof
  deriving (Eq)

instance Show Token where
  show (TokenLet pos) = show "let" ++ prettyAlexPosn pos
  show (TokenIn pos) = show "in" ++ prettyAlexPosn pos
  show (TokenWhile pos) = show "while" ++ prettyAlexPosn pos
  show (TokenFor pos) = show "for" ++ prettyAlexPosn pos
  show (TokenTo pos) = show "to" ++ prettyAlexPosn pos
  show (TokenBreak pos) = show "break" ++ prettyAlexPosn pos
  show (TokenEnd pos) = show "end" ++ prettyAlexPosn pos
  show (TokenFunction pos) = show "function" ++ prettyAlexPosn pos
  show (TokenVar pos) = show "var" ++ prettyAlexPosn pos
  show (TokenType pos) = show "type" ++ prettyAlexPosn pos
  show (TokenArray pos) = show "array" ++ prettyAlexPosn pos
  show (TokenIf pos) = show "if" ++ prettyAlexPosn pos
  show (TokenThen pos) = show "then" ++ prettyAlexPosn pos
  show (TokenElse pos) = show "else" ++ prettyAlexPosn pos
  show (TokenDo pos) = show "do" ++ prettyAlexPosn pos
  show (TokenOf pos) = show "of" ++ prettyAlexPosn pos
  show (TokenNil pos) = show "nil" ++ prettyAlexPosn pos
  show (TokenName (s, pos)) = show s ++ prettyAlexPosn pos
  show (TokenInt (n, pos)) = show n ++ prettyAlexPosn pos
  show (TokenString (s, pos)) = show s ++ prettyAlexPosn pos
  show (TokenComma pos) = show "," ++ prettyAlexPosn pos
  show (TokenDot pos) = show "." ++ prettyAlexPosn pos
  show (TokenColon pos) = show ":" ++ prettyAlexPosn pos
  show (TokenSemicolon pos) = show ";" ++ prettyAlexPosn pos
  show (TokenEq pos) = show "=" ++ prettyAlexPosn pos
  show (TokenPlus pos) = show "+" ++ prettyAlexPosn pos
  show (TokenMinus pos) = show "-" ++ prettyAlexPosn pos
  show (TokenTimes pos) = show "*" ++ prettyAlexPosn pos
  show (TokenDiv pos) = show "/" ++ prettyAlexPosn pos
  show (TokenNeq pos) = show "<>" ++ prettyAlexPosn pos
  show (TokenLess pos) = show "<" ++ prettyAlexPosn pos
  show (TokenLeq pos) = show "<=" ++ prettyAlexPosn pos
  show (TokenGreater pos) = show ">" ++ prettyAlexPosn pos
  show (TokenGeq pos) = show ">=" ++ prettyAlexPosn pos
  show (TokenAnd pos) = show "&"  ++ prettyAlexPosn pos
  show (TokenPipe pos) = show "|" ++ prettyAlexPosn pos
  show (TokenAssign pos) = show ":=" ++ prettyAlexPosn pos
  show (TokenLParen pos) = show "(" ++ prettyAlexPosn pos
  show (TokenRParen pos) = show ")" ++ prettyAlexPosn pos
  show (TokenLBrace pos) = show "{" ++ prettyAlexPosn pos
  show (TokenRBrace pos) = show "}" ++ prettyAlexPosn pos
  show (TokenLBracket pos) = show "[" ++ prettyAlexPosn pos
  show (TokenRBracket pos) = show "]" ++ prettyAlexPosn pos
  show (Eof) = show "[EOF]"

prettyAlexPosn (AlexPn offset line col) = 
  "at line " ++ show line ++ ", col " ++ show col

-- lexer = alexScanTokens
lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)
}