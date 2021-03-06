{
module Parse.Lexer (
  Parse.Lexer.lexer, 
  )  
where
import Parse.Data
}

%wrapper "basic"
$digit = 0-9            -- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "//".*				;
  let					{ \s -> TokenLet }
  in					{ \s -> TokenIn }
  $digit+				{ \s -> TokenInt (read s) }
  [\=] { \s ->TokenEq}
  [\+] { \s -> TokenPlus}
  [\-] { \s -> TokenMinus}
  [\*] { \s -> TokenTimes}
  [\/] { \s -> TokenDiv}
  [\(] { \s -> TokenOB}
  [\)] { \s -> TokenCB}

  $alpha [$alpha $digit \_ \']*		{ \s -> TokenVar s }

{
-- Each action has type :: String -> Token


lexer = alexScanTokens
}