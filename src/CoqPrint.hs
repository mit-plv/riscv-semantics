import ParseToJson hiding (main)
import Debug.Trace (trace)
import Control.Monad
import Data.List

coqPrint (If cond ifT ifF) =
    (++ "\n") . join . (intersperse " ")  $ ["if", coqPrint cond, "then", coqPrint ifT, "else" , coqPrint ifF]
coqPrint (Arith TokenMPLUS l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " + " $ fmap coqPrint l
coqPrint (Arith TokenMTIMES l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " * " $ fmap coqPrint l
coqPrint (Arith TokenMMINUS [a]) = "(-" ++ coqPrint a ++")"
coqPrint (Arith TokenMMINUS l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " - " $ fmap coqPrint l
coqPrint (Arith TokenMOR l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " || " $ fmap coqPrint l
coqPrint (Arith TokenMAND l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " && " $ fmap coqPrint l
coqPrint (Arith TokenMBAND l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " and " $ fmap coqPrint l
coqPrint (Arith TokenMBOR l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " or " $ fmap coqPrint l
coqPrint (Arith TokenMTYPEOF l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " : " $ fmap coqPrint l
coqPrint (Arith TokenMEQUAL l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " == " $ fmap coqPrint l
coqPrint (Arith TokenMDIFF l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " /= " $ fmap coqPrint l
coqPrint (Arith TokenMLT l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " < " $ fmap coqPrint l
coqPrint (Arith TokenMGT l) =
   (\l-> "(" ++ l ++ ")") . join . intersperse " > " $ fmap coqPrint l
coqPrint (App a b) =
    join . intersperse " " $ ["(", coqPrint a, ")", "(", coqPrint b, ")"]
coqPrint (Num a) = show a
coqPrint (Iden s) = s
coqPrint (Do ((Bind name b):q)) = join . intersperse " " $ [coqPrint b, ">>=\n", "(fun", name,"=>", coqPrint (Do q),")"]
coqPrint (Do ([t])) = join . intersperse " " $ ["(", coqPrint t, ")"]
coqPrint (Do (t:q)) = join . intersperse " " $ ["(", coqPrint t, ">>\n",  coqPrint (Do q),")"]
coqPrint (Do []) = ""
coqPrint (Let s l body) = join . intersperse " " $ ["let", s, "=", nestedIf l, "in\n", coqPrint body]
coqPrint x = trace (show x) undefined


printMatcher (App a b) = join . intersperse " " $ [printMatcher a, printMatcher b]
printMatcher (Iden s) = s


executeCoq ((ExecuteCase a b):q) = join . intersperse " " $ ["|", printMatcher a, "=>", coqPrint b, "\n", executeCoq q]
executeCoq [] = ""

nestedIf [(Iden "otherwise",t)] = coqPrint t
nestedIf ((c,t):q) = join .intersperse " " $ ["(","if", coqPrint c, "then\n", coqPrint t, "else\n", nestedIf q, ")"]


transliterateCoq l ="Definition execute (instr: Instruction) : Riscv :=\nmatch instr with\n" ++ executeCoq l ++ "\nend."

main = undefined
