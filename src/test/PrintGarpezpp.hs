{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintGarpezpp.
--   Generated by the BNF converter.

module PrintGarpezpp where

import qualified AbsGarpezpp
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsGarpezpp.Ident where
  prt _ (AbsGarpezpp.Ident i) = doc (showString i)

instance Print AbsGarpezpp.Program where
  prt i e = case e of
    AbsGarpezpp.Prog dlists fdecls -> prPrec i 0 (concatD [prt 0 dlists, prt 0 fdecls])

instance Print AbsGarpezpp.FDecl where
  prt i e = case e of
    AbsGarpezpp.FDecl rtype id params block -> prPrec i 0 (concatD [prt 0 rtype, prt 0 id, doc (showString "("), prt 0 params, doc (showString ")"), prt 0 block])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsGarpezpp.Param where
  prt i e = case e of
    AbsGarpezpp.Param type_ passby id -> prPrec i 0 (concatD [prt 0 type_, prt 0 passby, prt 0 id])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsGarpezpp.PassBy where
  prt i e = case e of
    AbsGarpezpp.PassVal -> prPrec i 0 (concatD [doc (showString "val")])
    AbsGarpezpp.PassRef -> prPrec i 0 (concatD [doc (showString "ref")])
    AbsGarpezpp.PassName -> prPrec i 0 (concatD [doc (showString "name")])
    AbsGarpezpp.PassRes -> prPrec i 0 (concatD [doc (showString "res")])
    AbsGarpezpp.PassValRes -> prPrec i 0 (concatD [doc (showString "valres")])
    AbsGarpezpp.PassConst -> prPrec i 0 (concatD [doc (showString "const")])

instance Print AbsGarpezpp.DList where
  prt i e = case e of
    AbsGarpezpp.VList type_ vdecls -> prPrec i 0 (concatD [prt 0 type_, prt 0 vdecls, doc (showString ";")])
    AbsGarpezpp.CList cdecls -> prPrec i 0 (concatD [doc (showString "const"), prt 0 cdecls, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsGarpezpp.VDecl where
  prt i e = case e of
    AbsGarpezpp.VSolo id -> prPrec i 0 (concatD [prt 0 id])
    AbsGarpezpp.VInit id rexp -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 rexp])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsGarpezpp.CDecl where
  prt i e = case e of
    AbsGarpezpp.CDecl id rexp -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 rexp])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsGarpezpp.Type where
  prt i e = case e of
    AbsGarpezpp.Type basic compound -> prPrec i 0 (concatD [prt 0 basic, prt 0 compound])

instance Print AbsGarpezpp.Compound where
  prt i e = case e of
    AbsGarpezpp.Simple -> prPrec i 0 (concatD [])
    AbsGarpezpp.Array compound rexp -> prPrec i 0 (concatD [prt 0 compound, doc (showString "["), prt 0 rexp, doc (showString "]")])
    AbsGarpezpp.Pointer compound -> prPrec i 0 (concatD [prt 0 compound, doc (showString "*")])

instance Print AbsGarpezpp.Basic where
  prt i e = case e of
    AbsGarpezpp.BBool -> prPrec i 0 (concatD [doc (showString "bool")])
    AbsGarpezpp.BChar -> prPrec i 0 (concatD [doc (showString "char")])
    AbsGarpezpp.BInt -> prPrec i 0 (concatD [doc (showString "int")])
    AbsGarpezpp.BFloat -> prPrec i 0 (concatD [doc (showString "float")])
    AbsGarpezpp.BString -> prPrec i 0 (concatD [doc (showString "string")])

instance Print AbsGarpezpp.RType where
  prt i e = case e of
    AbsGarpezpp.RVoid -> prPrec i 0 (concatD [doc (showString "void")])
    AbsGarpezpp.RBasic basic -> prPrec i 0 (concatD [prt 0 basic])
    AbsGarpezpp.RRef type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "&")])

instance Print AbsGarpezpp.Block where
  prt i e = case e of
    AbsGarpezpp.Block dlists stms -> prPrec i 0 (concatD [doc (showString "{"), prt 0 dlists, prt 0 stms, doc (showString "}")])

instance Print AbsGarpezpp.Stm where
  prt i e = case e of
    AbsGarpezpp.StmBlock block -> prPrec i 0 (concatD [prt 0 block])
    AbsGarpezpp.StmCall id rexps -> prPrec i 0 (concatD [prt 0 id, doc (showString "("), prt 0 rexps, doc (showString ")"), doc (showString ";")])
    AbsGarpezpp.PredW pwrite rexp -> prPrec i 0 (concatD [prt 0 pwrite, doc (showString "("), prt 0 rexp, doc (showString ")"), doc (showString ";")])
    AbsGarpezpp.Assign lexp assignop rexp -> prPrec i 0 (concatD [prt 0 lexp, prt 0 assignop, prt 0 rexp, doc (showString ";")])
    AbsGarpezpp.StmL lexp -> prPrec i 0 (concatD [prt 0 lexp, doc (showString ";")])
    AbsGarpezpp.If rexp stm -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 rexp, doc (showString ")"), prt 0 stm])
    AbsGarpezpp.IfElse rexp stm1 stm2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 rexp, doc (showString ")"), prt 0 stm1, doc (showString "else"), prt 0 stm2])
    AbsGarpezpp.While rexp stm -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 rexp, doc (showString ")"), prt 0 stm])
    AbsGarpezpp.DoWhile stm rexp -> prPrec i 0 (concatD [doc (showString "do"), prt 0 stm, doc (showString "while"), doc (showString "("), prt 0 rexp, doc (showString ")"), doc (showString ";")])
    AbsGarpezpp.For id rexp1 dir rexp2 stm -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 id, doc (showString "="), prt 0 rexp1, prt 0 dir, prt 0 rexp2, doc (showString ")"), prt 0 stm])
    AbsGarpezpp.JmpStm jump -> prPrec i 0 (concatD [prt 0 jump, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsGarpezpp.Dir where
  prt i e = case e of
    AbsGarpezpp.UpTo -> prPrec i 0 (concatD [doc (showString "upto")])
    AbsGarpezpp.DownTo -> prPrec i 0 (concatD [doc (showString "downto")])

instance Print AbsGarpezpp.Jump where
  prt i e = case e of
    AbsGarpezpp.Return -> prPrec i 0 (concatD [doc (showString "return")])
    AbsGarpezpp.ReturnE rexp -> prPrec i 0 (concatD [doc (showString "return"), prt 0 rexp])
    AbsGarpezpp.Break -> prPrec i 0 (concatD [doc (showString "break")])
    AbsGarpezpp.Continue -> prPrec i 0 (concatD [doc (showString "continue")])

instance Print AbsGarpezpp.LExp where
  prt i e = case e of
    AbsGarpezpp.Deref lexp -> prPrec i 0 (concatD [doc (showString "*"), prt 0 lexp])
    AbsGarpezpp.Post lexp incdecop -> prPrec i 1 (concatD [prt 2 lexp, prt 0 incdecop])
    AbsGarpezpp.Pre incdecop lexp -> prPrec i 2 (concatD [prt 0 incdecop, prt 3 lexp])
    AbsGarpezpp.Access lexp rexp -> prPrec i 3 (concatD [prt 3 lexp, doc (showString "["), prt 0 rexp, doc (showString "]")])
    AbsGarpezpp.Name id -> prPrec i 4 (concatD [prt 0 id])

instance Print AbsGarpezpp.RExp where
  prt i e = case e of
    AbsGarpezpp.Or rexp1 rexp2 -> prPrec i 0 (concatD [prt 0 rexp1, doc (showString "||"), prt 1 rexp2])
    AbsGarpezpp.And rexp1 rexp2 -> prPrec i 1 (concatD [prt 1 rexp1, doc (showString "&&"), prt 2 rexp2])
    AbsGarpezpp.Not rexp -> prPrec i 2 (concatD [doc (showString "!"), prt 3 rexp])
    AbsGarpezpp.Comp rexp1 compop rexp2 -> prPrec i 3 (concatD [prt 3 rexp1, prt 0 compop, prt 4 rexp2])
    AbsGarpezpp.Add rexp1 rexp2 -> prPrec i 4 (concatD [prt 4 rexp1, doc (showString "+"), prt 5 rexp2])
    AbsGarpezpp.Sub rexp1 rexp2 -> prPrec i 4 (concatD [prt 4 rexp1, doc (showString "-"), prt 5 rexp2])
    AbsGarpezpp.Mul rexp1 rexp2 -> prPrec i 5 (concatD [prt 5 rexp1, doc (showString "*"), prt 6 rexp2])
    AbsGarpezpp.Div rexp1 rexp2 -> prPrec i 5 (concatD [prt 5 rexp1, doc (showString "/"), prt 6 rexp2])
    AbsGarpezpp.Rem rexp1 rexp2 -> prPrec i 5 (concatD [prt 5 rexp1, doc (showString "%"), prt 6 rexp2])
    AbsGarpezpp.Pow rexp1 rexp2 -> prPrec i 6 (concatD [prt 7 rexp1, doc (showString "^"), prt 6 rexp2])
    AbsGarpezpp.Sign signop rexp -> prPrec i 7 (concatD [prt 0 signop, prt 8 rexp])
    AbsGarpezpp.Ref lexp -> prPrec i 7 (concatD [doc (showString "&"), prt 0 lexp])
    AbsGarpezpp.RLExp lexp -> prPrec i 8 (concatD [prt 0 lexp])
    AbsGarpezpp.ArrList rexps -> prPrec i 9 (concatD [doc (showString "["), prt 0 rexps, doc (showString "]")])
    AbsGarpezpp.FCall id rexps -> prPrec i 9 (concatD [prt 0 id, doc (showString "("), prt 0 rexps, doc (showString ")")])
    AbsGarpezpp.PredR pread -> prPrec i 9 (concatD [prt 0 pread, doc (showString "("), doc (showString ")")])
    AbsGarpezpp.Lit literal -> prPrec i 10 (concatD [prt 0 literal])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsGarpezpp.PRead where
  prt i e = case e of
    AbsGarpezpp.ReadChar -> prPrec i 0 (concatD [doc (showString "readChar")])
    AbsGarpezpp.ReadInt -> prPrec i 0 (concatD [doc (showString "readInt")])
    AbsGarpezpp.ReadFloat -> prPrec i 0 (concatD [doc (showString "readFloat")])
    AbsGarpezpp.ReadString -> prPrec i 0 (concatD [doc (showString "readString")])

instance Print AbsGarpezpp.PWrite where
  prt i e = case e of
    AbsGarpezpp.WriteChar -> prPrec i 0 (concatD [doc (showString "writeChar")])
    AbsGarpezpp.WriteInt -> prPrec i 0 (concatD [doc (showString "writeInt")])
    AbsGarpezpp.WriteFloat -> prPrec i 0 (concatD [doc (showString "writeFloat")])
    AbsGarpezpp.WriteString -> prPrec i 0 (concatD [doc (showString "writeString")])

instance Print AbsGarpezpp.AssignOp where
  prt i e = case e of
    AbsGarpezpp.AssignEq -> prPrec i 0 (concatD [doc (showString "=")])
    AbsGarpezpp.AssignAdd -> prPrec i 0 (concatD [doc (showString "+=")])
    AbsGarpezpp.AssignSub -> prPrec i 0 (concatD [doc (showString "-=")])
    AbsGarpezpp.AssignMul -> prPrec i 0 (concatD [doc (showString "*=")])
    AbsGarpezpp.AssignDiv -> prPrec i 0 (concatD [doc (showString "/=")])
    AbsGarpezpp.AssignMod -> prPrec i 0 (concatD [doc (showString "%=")])

instance Print AbsGarpezpp.CompOp where
  prt i e = case e of
    AbsGarpezpp.Lt -> prPrec i 0 (concatD [doc (showString "<")])
    AbsGarpezpp.Leq -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsGarpezpp.Eq -> prPrec i 0 (concatD [doc (showString "==")])
    AbsGarpezpp.Neq -> prPrec i 0 (concatD [doc (showString "!=")])
    AbsGarpezpp.Geq -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsGarpezpp.Gt -> prPrec i 0 (concatD [doc (showString ">")])

instance Print AbsGarpezpp.IncDecOp where
  prt i e = case e of
    AbsGarpezpp.Inc -> prPrec i 0 (concatD [doc (showString "++")])
    AbsGarpezpp.Dec -> prPrec i 0 (concatD [doc (showString "--")])

instance Print AbsGarpezpp.SignOp where
  prt i e = case e of
    AbsGarpezpp.Pos -> prPrec i 0 (concatD [doc (showString "+")])
    AbsGarpezpp.Neg -> prPrec i 0 (concatD [doc (showString "-")])

instance Print AbsGarpezpp.Literal where
  prt i e = case e of
    AbsGarpezpp.LBool boolean -> prPrec i 0 (concatD [prt 0 boolean])
    AbsGarpezpp.LChar c -> prPrec i 0 (concatD [prt 0 c])
    AbsGarpezpp.LInt n -> prPrec i 0 (concatD [prt 0 n])
    AbsGarpezpp.LFloat d -> prPrec i 0 (concatD [prt 0 d])
    AbsGarpezpp.LString str -> prPrec i 0 (concatD [prt 0 str])

instance Print AbsGarpezpp.Boolean where
  prt i e = case e of
    AbsGarpezpp.BFalse -> prPrec i 0 (concatD [doc (showString "false")])
    AbsGarpezpp.BTrue -> prPrec i 0 (concatD [doc (showString "true")])

instance Print [AbsGarpezpp.Param] where
  prt = prtList

instance Print [AbsGarpezpp.FDecl] where
  prt = prtList

instance Print [AbsGarpezpp.VDecl] where
  prt = prtList

instance Print [AbsGarpezpp.CDecl] where
  prt = prtList

instance Print [AbsGarpezpp.DList] where
  prt = prtList

instance Print [AbsGarpezpp.Stm] where
  prt = prtList

instance Print [AbsGarpezpp.RExp] where
  prt = prtList

