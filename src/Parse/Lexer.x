{
module Parse.Lexer (
  lexer, 
  Token(..),
  Alex,
  alexError,
  runAlex
  )  
where
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]		-- alphabetic characters

-- each token functions needs AlexInput:{AlexPosn, Char, [Byte], String} -> Int -> AlexToken
tokens :-
  $white+				;
  "//".*				;
  let					{ mkL LLet }
  in					{ mkL LIn }
  $digit+				{ mkL LInt }
  $alpha [$alpha $digit \_ \']*		{ mkL LVar}
  [\=] { mkL LEq}
  [\+] { mkL LPlus }
  [\-] { mkL LMinus }
  [\*] { mkL LTimes }
  [\/] { mkL LDiv }
  [\(] { mkL LLParen }
  [\)] { mkL LRParen }


{

data LexemeClass = LInt | LVar | LLet | LIn | LLParen | LRParen 
  | LEq | LPlus | LMinus | LTimes | LDiv
  deriving (Eq, Show)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (pos, _, _, str) len = 
  case c of
    LInt -> return (TokenInt ((read tok), pos))
    LVar -> return (TokenVar (tok, pos))
    LLet -> return (TokenLet pos)
    LIn -> return (TokenIn pos)
    LLParen -> return (TokenLParen pos)
    LRParen -> return (TokenRParen pos)
    LEq -> return (TokenEq pos)
    LPlus -> return (TokenPlus pos)
    LMinus -> return (TokenMinus pos)
    LTimes -> return (TokenTimes pos)
    LDiv -> return (TokenDiv pos)
  where
    tok = take len str


alexEOF :: Alex Token
alexEOF = return Eof


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
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | Eof
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
  show (TokenLParen pos) = show "(" ++ prettyAlexPosn pos
  show (TokenRParen pos) = show ")" ++ prettyAlexPosn pos
  show (Eof) = show "[EOF]"

prettyAlexPosn (AlexPn offset line col) = 
  "at line " ++ show line ++ ", col " ++ show col

-- lexer = alexScanTokens
lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)
}