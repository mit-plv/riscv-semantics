{
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Char
import GHC.Generics
import Generics.Generic.Aeson
import Data.Aeson

}
%name riscv
%tokentype {Token}
%error { parseError}
%token
--field
 field {TokenFIELD $$}
--Instr token
 instrToken {TokenINSTR $$}
 --RVPrimitive:
 nl {TokenNl}
 primToken {TokenPRIM $$}
 if {TokenMIF}
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
 do {TokenMDO}
 ident {TokenMVar $$}
 num {TokenMNum $$}

%nonassoc field instrToken primToken ident num if '('
%nonassoc APP
%%
TotExecute : Execute nl TotExecute {$1:$3}
| Execute {[$1]}

Execute : execute Exp '=' do nl Assignments {ExecuteCase $2 $6}
        | execute Exp '=' Exp {ExecuteCase $2 [Return $4]}

Assignment : ident '<-' Exp {Bind ($1) $3}
           |  Exp {Return $1}

Assignments : Assignment nl Assignments {$1 : $3}
           | Assignment {[$1]}

Exp : Exp Exp %prec APP {App $1 $2}
    | '(' Exp ')' {Paren $2}
    | Exp '+' Exp {Arith TokenMPLUS [$1,$3]}
    | Exp '-' Exp {Arith TokenMMINUS [$1,$3]}
    | Exp '==' Exp {Arith TokenMEQUAL [$1,$3]}
    | Exp '/=' Exp {Arith TokenMDIFF [$1,$3]}
    | Exp '<' Exp {Arith TokenMLT [$1,$3]}
    | Exp '>' Exp {Arith TokenMGT [$1,$3]}
    | instrToken {Atom $1}
    | field {Atom $1}
    | primToken {Atom $1}
    | ident {Var $1}
    | num {Num $1}
    | if Exp then Exp else Exp {If $2 $4 $6}

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
    = Paren Exp
      | App Exp Exp
      | Atom String
      | Var String
      | Num Integer
      | If Exp Exp Exp --When is syntaxic sugar
      | Arith Token [Exp]
  deriving (Show,Generic)

instance ToJSON Exp where toJSON = gtoJson
data Token =
  TokenNl
    --field
   | TokenFIELD String
     --INSTR 
   | TokenINSTR String
     -- RVPrimitive
   | TokenPRIM String
     -- Control language
--alphanum
   | TokenMIF
   | TokenMTHEN
   | TokenMELSE
   | TokenMEXECUTE
   | TokenMDO
   | TokenMVar String
-- lexerNumber
   | TokenMNum Integer
   --lexercustom
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
 --RVPrimitive:
      ("getRegister",rest) -> (TokenPRIM "getRegister")  : lexer rest
      ("setRegister",rest) -> (TokenPRIM "setRegister")  : lexer rest
      ("loadByte",rest) -> (TokenPRIM "loadByte")  : lexer rest
      ("loadHalf",rest) -> (TokenPRIM "loadHalf")  : lexer rest
      ("loadWord",rest) -> (TokenPRIM "loadWord")  : lexer rest
      ("storeByte",rest) -> (TokenPRIM "storeByte")  : lexer rest
      ("storeHalf",rest) -> (TokenPRIM "storeHalf")  : lexer rest
      ("storeWord",rest) -> (TokenPRIM "storeWord")  : lexer rest
      ("loadCSR",rest) -> (TokenPRIM "loadCSR")  : lexer rest
      ("storeCSR",rest) -> (TokenPRIM "storeCSR")  : lexer rest
      ("getPC",rest) -> (TokenPRIM "getPC")  : lexer rest
      ("setPC",rest) -> (TokenPRIM "setPC")  : lexer rest
    -- Instructions:
      ("Lui", rest) -> (TokenINSTR "Lui") : lexer rest
      ("Auipc", rest) -> (TokenINSTR "Auipc") : lexer rest
      ("Jal", rest) -> (TokenINSTR "Jal") : lexer rest
      ("Jalr", rest) -> (TokenINSTR "Jalr") : lexer rest
      ("Beq", rest) -> (TokenINSTR "Beq") : lexer rest
      ("Bne", rest) -> (TokenINSTR "Bne") : lexer rest
      ("Blt", rest) -> (TokenINSTR "Blt") : lexer rest
      ("Bge", rest) -> (TokenINSTR "Bge") : lexer rest
      ("Bltu", rest) -> (TokenINSTR "Bltu") : lexer rest
      ("Bgeu", rest) -> (TokenINSTR "Bgeu") : lexer rest
      ("Lb", rest) -> (TokenINSTR "Lb") : lexer rest
      ("Lh", rest) -> (TokenINSTR "Lh") : lexer rest
      ("Lw", rest) -> (TokenINSTR "Lw") : lexer rest
      ("Lbu", rest) -> (TokenINSTR "Lbu") : lexer rest
      ("Lhu", rest) -> (TokenINSTR "Lhu") : lexer rest
      ("Sb", rest) -> (TokenINSTR "Sb") : lexer rest
      ("Sh", rest) -> (TokenINSTR "Sh") : lexer rest
      ("Sw", rest) -> (TokenINSTR "Sw") : lexer rest
      ("Addi", rest) -> (TokenINSTR "Addi") : lexer rest
      ("Slti", rest) -> (TokenINSTR "Slti") : lexer rest
      ("Sltiu", rest) -> (TokenINSTR "Sltiu") : lexer rest
      ("Xori", rest) -> (TokenINSTR "Xori") : lexer rest
      ("Ori", rest) -> (TokenINSTR "Ori") : lexer rest
      ("Andi", rest) -> (TokenINSTR "Andi") : lexer rest
      ("Slli", rest) -> (TokenINSTR "Slli") : lexer rest
      ("Srli", rest) -> (TokenINSTR "Srli") : lexer rest
      ("Srai", rest) -> (TokenINSTR "Srai") : lexer rest
      ("Add", rest) -> (TokenINSTR "Add") : lexer rest
      ("Sub", rest) -> (TokenINSTR "Sub") : lexer rest
      ("Sll", rest) -> (TokenINSTR "Sll") : lexer rest
      ("Slt", rest) -> (TokenINSTR "Slt") : lexer rest
      ("Sltu", rest) -> (TokenINSTR "Sltu") : lexer rest
      ("Xor", rest) -> (TokenINSTR "Xor") : lexer rest
      ("Srl", rest) -> (TokenINSTR "Srl") : lexer rest
      ("Sra", rest) -> (TokenINSTR "Sra") : lexer rest
      ("Or", rest) -> (TokenINSTR "Or") : lexer rest
      ("And", rest) -> (TokenINSTR "And") : lexer rest
      ("Fence", rest) -> (TokenINSTR "Fence") : lexer rest
      ("Fence", rest) -> (TokenINSTR "Fence") : lexer rest
      ("Mul", rest) -> (TokenINSTR "Mul") : lexer rest
      ("Mulh", rest) -> (TokenINSTR "Mulh") : lexer rest
      ("Mulhsu", rest) -> (TokenINSTR "Mulhsu") : lexer rest
      ("Mulhu", rest) -> (TokenINSTR "Mulhu") : lexer rest
      ("Div", rest) -> (TokenINSTR "Div") : lexer rest
      ("Divu", rest) -> (TokenINSTR "Divu") : lexer rest
      ("Rem", rest) -> (TokenINSTR "Rem") : lexer rest
      ("Remu", rest) -> (TokenINSTR "Remu") : lexer rest
      ("Ecall", rest) -> (TokenINSTR "Ecall") : lexer rest
      ("Ebreak", rest) -> (TokenINSTR "Ebreak") : lexer rest
      ("Uret", rest) -> (TokenINSTR "Uret") : lexer rest
      ("Sret", rest) -> (TokenINSTR "Sret") : lexer rest
      ("Hret", rest) -> (TokenINSTR "Hret") : lexer rest
      ("Mret", rest) -> (TokenINSTR "Mret") : lexer rest
      ("Dret", rest) -> (TokenINSTR "Dret") : lexer rest
      ("Sfence_vm", rest) -> (TokenINSTR "Sfence_vm") : lexer rest
      ("Wfi", rest) -> (TokenINSTR "Wfi") : lexer rest
      ("Csrrw", rest) -> (TokenINSTR "Csrrw") : lexer rest
      ("Csrrs", rest) -> (TokenINSTR "Csrrs") : lexer rest
      ("Csrrc", rest) -> (TokenINSTR "Csrrc") : lexer rest
      ("Csrrwi", rest) -> (TokenINSTR "Csrrwi") : lexer rest
      ("Csrrsi", rest) -> (TokenINSTR "Csrrsi") : lexer rest
      ("Csrrci", rest) -> (TokenINSTR "Csrrci") : lexer rest
      ("rd", rest) -> (TokenFIELD "rd") : lexer rest
      ("rs1", rest) -> (TokenFIELD "rs1") : lexer rest
      ("rs2", rest) -> (TokenFIELD "rs2") : lexer rest
      ("pred", rest) -> (TokenFIELD "pred") : lexer rest
      ("succ", rest) -> (TokenFIELD "succ") : lexer rest
      ("imm20", rest) -> (TokenFIELD "imm20") : lexer rest
      ("oimm20", rest) -> (TokenFIELD "oimm20") : lexer rest
      ("jimm20", rest) -> (TokenFIELD "jimm20") : lexer rest
      ("imm12", rest) -> (TokenFIELD "imm12") : lexer rest
      ("oimm12", rest) -> (TokenFIELD "oimm12") : lexer rest
      ("csr12", rest) -> (TokenFIELD "csr12") : lexer rest
      ("simm12", rest) -> (TokenFIELD "simm12") : lexer rest
      ("sbimm12", rest) -> (TokenFIELD "sbimm12") : lexer rest
      ("shamt5", rest) -> (TokenFIELD "shamt5") : lexer rest
      ("zimm", rest) -> (TokenFIELD "zimm") : lexer rest
                       -- control
      ("execute", rest) -> TokenMEXECUTE : lexer rest
      ("if", rest) -> TokenMIF : lexer rest
      ("then", rest) -> TokenMTHEN : lexer rest
      ("else", rest) -> TokenMELSE : lexer rest
      ("do", rest) -> TokenMDO: lexer rest
      (varname, rest) -> TokenMVar varname : lexer rest
main = getContents >>= print. encode . riscv . lexer
}

