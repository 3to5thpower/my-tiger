{
module Parse.Parser (
      parse,
      Exp(..),
) where
import Parse.Lexer
}


%name parser
%lexer {lexer} {Eof}
%monad {Alex}
%tokentype {Token}
%error {parseError}

%token
    let             { TokenLet _ }
    in              { TokenIn _ }
    int             { TokenInt ($$, _) }
    var             { TokenVar ($$, _) }
    '='             { TokenEq _ }
    '+'             { TokenPlus _ }
    '-'             { TokenMinus _ }
    '*'             { TokenTimes _ }
    '/'             { TokenDiv _ }
    '('             { TokenLParen _ }
    ')'             { TokenRParen _ }

%right in
%left '+' '-' 
%left '*' '/'
%%

Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
    | Exp '+' Exp {Plus $1 $3}
    | Exp '-' Exp {Minus $1 $3}
    | Exp '*' Exp {Times $1 $3}
    | Exp '/' Exp {Div $1 $3}
    | '(' Exp ')' { $2 }
    | int { Int $1 }
    | var { Var $1 }

{
data Exp = 
    Let String Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Brack Exp
  | Int Int
  | Var String
  deriving(Eq, Show)

parseError :: Token -> Alex a
parseError tok = alexError $ "Parse Error: " ++ show tok

parse s = runAlex s parser
}
