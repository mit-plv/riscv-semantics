{
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Char
import GHC.Generics
import Generics.Generic.Aeson
import Data.Aeson
import Data.List
import Data.Maybe
}

%name riscv
%tokentype {Token}
%error {parseError}
%token
 nl {TokenNl}
 if {TokenMIF}
 when {TokenMWHEN}
 then {TokenMTHEN}
 else {TokenMELSE}
 execute {TokenMEXECUTE}
 '(' {TokenMLPAREN}
 ')' {TokenMRPAREN}
 '+' {TokenMPLUS}
 '-' {TokenMMINUS}
 '==' {TokenMEQUAL}
 '$' {TokenMAPPLY}
 '<-' {TokenMBIND}
 '/=' {TokenMDIFF}
 '<' {TokenMLT}
 '>' {TokenMGT}
 '=' {TokenMDEFINE}
 '::' {TokenMTYPEOF}
 '.|.' {TokenMOR}
 '.&.' {TokenMAND}
 do {TokenMDO}
 ident {TokenMVar $$}
 num {TokenMNum $$}

%nonassoc ident num if when '('
%nonassoc APP
%%
TotExecute  : Execute nl TotExecute {$1:$3}
| Execute TotExecute {$1:$2}
| {[]}

Execute : execute Exp '=' do nl Assignments {ExecuteCase $2 $6}
        | execute Exp '=' Exp {ExecuteCase $2 [Return $4]}

Assignment : ident '<-' Exp {Bind ($1) $3}
           |  Exp {Return $1}

Assignments : Assignment nl Assignments {$1 : $3}
           | {[]}

Exp : Exp Exp %prec APP {App $1 $2}
    | '(' Exp ')' {$2}
    | Exp '+' Exp {Arith TokenMPLUS [$1,$3]}
    | Exp '.|.' Exp {Arith TokenMOR [$1,$3]}
    | Exp '.&.' Exp {Arith TokenMAND [$1,$3]}
    | Exp '::' Exp {Arith TokenMTYPEOF [$1,$3]}
    | Exp '-' Exp {Arith TokenMMINUS [$1,$3]}
    | Exp '==' Exp {Arith TokenMEQUAL [$1,$3]}
    | Exp '/=' Exp {Arith TokenMDIFF [$1,$3]}
    | Exp '<' Exp {Arith TokenMLT [$1,$3]}
    | Exp '>' Exp {Arith TokenMGT [$1,$3]}
    | ident {Iden $1}
    | num {Num $1}
    | if Exp then Exp else Exp {If $2 $4 $6}
    | when Exp Exp {If $2 $3 (Iden "noAction")}

{

parseError _ = error "Parse error"
type TotExecute = [Execute]

data Execute =
   ExecuteCase Exp Assignments
  deriving(Show,Generic)

instance ToJSON Execute where toJSON = gtoJson

type Assignments = [Assignment]

data Assignment =
    Bind String Exp
    | Return Exp
  deriving(Show,Generic)

instance ToJSON Assignment where toJSON = gtoJson

data Exp
    = App Exp Exp
      | Iden String
      | Num Integer
      | If Exp Exp Exp --When is syntaxic sugar
      | Arith Token [Exp]
  deriving (Show,Generic)

instance ToJSON Exp where toJSON = gtoJson
data Token =
  TokenNl
   | TokenMWHEN
   | TokenMIF
   | TokenMTHEN
   | TokenMELSE
   | TokenMEXECUTE
   | TokenMDO
   | TokenMVar String
   | TokenMNum Integer
   | TokenMEQUAL
   | TokenMDIFF
   | TokenMDEFINE
   | TokenMLPAREN
   | TokenMRPAREN
   | TokenMPLUS
   | TokenMMINUS
   | TokenMBIND
   | TokenMLT
   | TokenMGT
   | TokenMAPPLY
   | TokenMAND
   | TokenMOR
   | TokenMTYPEOF
--   | TokenLEQ
--   | TokenGEQ
  deriving (Show,Generic)
instance ToJSON Token where toJSON = gtoJson

lexer :: String -> [Token]
lexer [] = []
lexer ('\n':cs) = TokenNl : lexer cs
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexerAlphaNumerical (c:cs)
      | isDigit c = lexerNumber (c:cs)
lexer ('=':'=':cs) = TokenMEQUAL : lexer cs
lexer ('/':'=':cs) = TokenMDIFF : lexer cs
lexer ('=':cs) = TokenMDEFINE : lexer cs
lexer ('(':cs) = TokenMLPAREN : lexer cs
lexer (')':cs) = TokenMRPAREN : lexer cs
lexer ('+':cs) = TokenMPLUS : lexer cs
lexer ('-':cs) = TokenMMINUS : lexer cs
lexer ('<':'-': cs) = TokenMBIND : lexer cs
--GEQ LEQ SHOULD BE HERE IF REQUIRED
lexer ('<':cs) = TokenMLT : lexer cs
lexer ('>':cs) = TokenMGT : lexer cs
lexer ('$':cs) = TokenMAPPLY : lexer cs
lexer ('.':'&':'.':cs) = TokenMAND : lexer cs
lexer ('.':'|':'.':cs) = TokenMOR : lexer cs
lexer (':':':':cs) = TokenMTYPEOF : lexer cs

lexerNumber cs =
    TokenMNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexerAlphaNumerical cs=
    case span isAlphaNum cs of
      ("execute", rest) -> TokenMEXECUTE : lexer rest
      ("if", rest) -> TokenMIF : lexer rest
      ("then", rest) -> TokenMTHEN : lexer rest
      ("else", rest) -> TokenMELSE : lexer rest
      ("do", rest) -> TokenMDO: lexer rest
      ("when",rest) -> TokenMWHEN: lexer rest
      (varname, rest) -> TokenMVar varname : lexer rest
dropUntil :: ([a] -> Bool) -> [a] -> [a]
dropUntil p l = if p l then l else dropUntil p $ tail l
stopWhen :: ([a] -> Bool) -> [a] -> [a]
stopWhen p [] = []
stopWhen p (h:t) = if (p(h:t)) then [] else h:(stopWhen p t)
main = getContents >>= print . riscv . lexer . drop 13 .  stopWhen (isPrefixOf "-- end ast") . dropUntil (isPrefixOf "-- begin ast")
}

