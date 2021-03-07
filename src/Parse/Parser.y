{
module Parse.Parser (
      parser,
) where
import Parse.Data
import Parse.Lexer
}


%name parser1
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
%%

Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
    |  Exp1 {Exp1 $1}

Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor			  
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' Exp ')'             { Brack $2 }

{
parseError :: Token -> Alex a
parseError tok = alexError $ "Parse Error: " ++ show tok

parser s = runAlex s parser1
}
