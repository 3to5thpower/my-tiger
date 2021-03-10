{
module Parse.Parser (
      parse,
      Decs,
      Dec(..),
      Type(..),
      TypeId(..),
      TyFields,
      VarDec(..),
      FunDec(..),
      Exp(..),
      LValue(..),
      Id(..),
) where
import Parse.Lexer
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
%left "|"
%left "&"
%nonassoc "<" ">" "<=" ">=" "=" "<>"
%left "+" "-"
%left "*" "/"
%left NEG
%%

Exp : LValue { LValue $1}
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
    | TypeId "{" records "}" { Record $1 $3}
    | TypeId "[" Exp "]" of Exp {Array $1 $3 $6} 
    | if Exp then Exp {IfThen $2 $4}
    | if Exp then Exp else Exp {IfThenElse $2 $4 $6}
    | while Exp do Exp {WhileDo $2 $4}
    | for Id ":=" Exp to Exp do Exp {ForToDo $2 $4 $6 $8}
    | break {Break}
    | let Decs in Exp end {LetInEnd $2 $4}
    | "(" Exp ")" {Brack $2}


Decs : {[]}
     | Decs Dec { $2 : $1 } 

Dec : type TypeId "=" Type { TyDec $2 $4 }
    | VarDec { VarDec $1 }
    | FunDec { FunDec $1 }
Type : TypeId { Type $1 }
     | "{" TyFields "}" { RecordType $2 }
     | array of TypeId { ArrayType $3 }
TyFields : {[]}
         | Id ":" TypeId { [($1, $3)]}
         | TyFields ","  Id ":" TypeId { ($3, $5) : $1 }
TypeId : id { TypeId $1 }
VarDec : var Id ":=" Exp {ShortVarDec $2 $4}
       | var Id ":" TypeId ":=" Exp {LongVarDec $2 $4 $6}
FunDec : function Id "(" TyFields ")" "=" Exp {ShortFunDec $2 $4 $7}
       | function Id "(" TyFields ")" ":" TypeId "=" Exp {LongFunDec $2 $4 $7 $9}


LValue : Id {Variable $1}
       | LValue "." Id {DotAccess $1 $3}
       | LValue "[" Exp "]" {Index $1 $3}
Id : id { Id $1}

sequencies : Exp {[$1]}
    | sequencies ";" Exp { $3 : $1 }
arguments : {[]}
    | Exp {[$1]}
    | arguments "," Exp { $3 : $1}
records : {[]}
    | Id "=" Exp {[($1, $3)]}
    | records "," Id "=" Exp {($3, $5) : $1}
{
type Decs = [Dec]
data Dec = TyDec TypeId Type
         | VarDec VarDec 
         | FunDec FunDec
         deriving (Eq, Show)
data Type = Type TypeId | RecordType TyFields | ArrayType TypeId deriving(Eq, Show)
newtype TypeId = TypeId String deriving (Eq, Show)
type TyFields = [(Id, TypeId)] 
data VarDec = ShortVarDec Id Exp
  | LongVarDec Id TypeId Exp
  deriving (Eq, Show)
data FunDec = ShortFunDec Id TyFields Exp
  | LongFunDec Id TyFields TypeId Exp
  deriving (Eq, Show)

data LValue =
    Variable Id  
  | DotAccess LValue Id
  | Index LValue Exp
  deriving (Eq, Show)
newtype Id = Id String deriving (Eq, Show)
data Exp = LValue LValue
  | Nil
  | Seq [Exp]
  | Unit
  | Int Int 
  | String String
  | FunCall Id [Exp]
  | Negate Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp 
  | Less Exp Exp
  | LessEqual Exp Exp
  | Greater Exp Exp
  | GreaterEqual Exp Exp
  | Equal Exp Exp
  | NotEqual Exp Exp
  | And Exp Exp
  | Or Exp Exp
  | Record TypeId [(Id, Exp)]
  | Array TypeId Exp Exp
  | IfThen Exp Exp
  | IfThenElse Exp Exp Exp
  | WhileDo Exp Exp
  | ForToDo Id Exp Exp Exp
  | Break
  | LetInEnd Decs Exp
  | Brack Exp
  deriving (Eq, Show)


parseError :: Token -> Alex a
parseError tok = alexError $ "Parse Error: " ++ show tok

parse s = runAlex s parser
}
