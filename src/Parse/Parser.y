{
module Parse.Parser (parse) where
import Parse.Lexer
import Parse.Data
}


%name parser
%lexer {lexer} {Eof}
%monad {Alex}
%tokentype {Token}
%error {parseError}

%token
    while           { TokenWhile _ }
    for { TokenFor _ } 
    to {TokenTo _ }
    break {TokenBreak _}
    end {TokenEnd _}
    function {TokenFunction _}
    var {TokenVar _}
    let             { TokenLet _ }
    in              { TokenIn _ }
    type            { TokenType _ }
    array {TokenArray _}
    of { TokenOf _ }
    if {TokenIf _}
    then {TokenThen _}
    else {TokenElse _}
    do {TokenDo _}
    nil {TokenNil _ }
    string { TokenString ($$, _) }
    int             { TokenInt ($$, _) }
    id             { TokenName ($$, _) }
    "," {TokenComma _}
    ":" {TokenColon _}
    ";" {TokenSemicolon _}
    "." {TokenDot _}
    "="             { TokenEq _ }
    "<>" { TokenNeq _ }
    "<" {TokenLess _}
    "<=" {TokenLeq _}
    ">" {TokenGreater _}
    ">=" {TokenGeq _}
    "&" {TokenAnd _}
    "|" {TokenPipe _}
    ":=" {TokenAssign _ }
    "+"             { TokenPlus _ }
    "-"             { TokenMinus _ }
    "*"             { TokenTimes _ }
    "/"             { TokenDiv _ }
    "("             { TokenLParen _ }
    ")"             { TokenRParen _ }
    "{" {TokenLBrace _}
    "}" {TokenRBrace _}
    "[" {TokenLBracket _}
    "]" {TokenRBracket _}
    

%right in
%right of
%nonassoc do
%nonassoc else
%nonassoc ':='
%left "|" "&"
%nonassoc "<" ">" "<=" ">=" "=" "<>"
%left "+" "-"
%left "*" "/"
%left NEG
%%

Program : Exp { $1 }

Decs : TyDec Decs {$1 : $2}
     | VarDec Decs { VarDec $1 : $2}
     | FunDec Decs {FunDec $1 : $2}
     | {[]}

TyDec : type Id "=" Type { TyDec $2 $4}

Type : Id { Type $1 }
     | "{" TyFields "}" { RecordType $2 }
     | array of Id { ArrayType $3 }
TyFields : {[]}
         | Id ":" Id { [($1, $3)]}
         | TyFields ","  Id ":" Id { $1 ++ [($3, $5)]}
VarDec : var Id ":=" Exp {ShortVarDec $2 $4}
       | var Id ":" Id ":=" Exp {LongVarDec $2 $4 $6}
FunDec : function Id "(" TyFields ")" "=" Exp {ShortFunDec $2 $4 $7}
       | function Id "(" TyFields ")" ":" Id "=" Exp {LongFunDec $2 $4 $7 $9}


LValue : Id {Variable $1}
       | Id "." Id { DotAccess (Variable $1) $3}
       | LValue "." Id { DotAccess $1 $3}
       | Id "[" Exp "]" {Index (Variable $1) $3}
       | LValue "[" Exp "]"  {Index $1 $3}

Id : id { Id $1}

Exp : LValue {LValue $1}
    | nil {Nil}
    | "(" sequencies ")" { Seq $2}
    | "(" ")" {Unit}
    | int {Int $1}
    | string {String $1}
    | Id "(" arguments ")" { FunCall $1 $3}
    | "-" Exp %prec NEG {Negate $2}
    | Exp "+" Exp {Plus $1 $3}
    | Exp "-" Exp {Minus $1 $3}
    | Exp "*" Exp {Times $1 $3}
    | Exp "/" Exp {Div $1 $3}
    | Exp "<" Exp {Less $1 $3}
    | Exp "<=" Exp {LessEqual $1 $3}
    | Exp ">" Exp {Greater $1 $3}
    | Exp ">=" Exp {GreaterEqual $1 $3}
    | Exp "=" Exp {Equal $1 $3}
    | Exp "<>" Exp {NotEqual $1 $3}
    | Exp "&" Exp {And $1 $3}
    | Exp "|" Exp {Or $1 $3}
    | Id "{" records "}" { Record $1 $3}
    | if Exp then Exp {IfThen $2 $4}
    | if Exp then Exp else Exp {IfThenElse $2 $4 $6}
    | while Exp do Exp {WhileDo $2 $4}
    | for Id ":=" Exp to Exp do Exp {ForToDo $2 $4 $6 $8}
    | break {Break}
    | let Decs in Exp end {LetInEnd $2 $4}
    | "(" Exp ")" {Brack $2}
    | Id "[" Exp "]" of Exp {Array $1 $3 $6} 

sequencies : Exp {[$1]}
    | sequencies ";" Exp { $1 ++ [$3] }
arguments : {[]}
    | Exp {[$1]}
    | arguments "," Exp { $1 ++ [$3] }
records : {[]}
    | Id "=" Exp {[($1, $3)]}
    | records "," Id "=" Exp {$1 ++ [($3, $5)]}

{
parseError :: Token -> Alex a
parseError tok = alexError $ "Parse Error: " ++ show tok

parse s = runAlex s parser
}
  