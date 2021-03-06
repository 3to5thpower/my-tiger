{
{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Parse.Lexer (
  Parse.Lexer.lexer, 
  Token(..)
  )  
where

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
-- The token type:
data Token
  = TokenLet
  | TokenIn
  | TokenVar String
  | TokenInt Int
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenOB
  | TokenCB
  deriving (Eq, Show)

lexer = alexScanTokens
}