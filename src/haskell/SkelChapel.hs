module SkelChapel where

-- Haskell module generated by the BNF converter

import AbsChapel
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Prog decls -> failure x
transDecl :: Decl -> Result
transDecl x = case x of
  FDecl ident forms intent type_ block -> failure x
  VList vdecls -> failure x
  CList cdecls -> failure x
transForm :: Form -> Result
transForm x = case x of
  Form intent ident type_ -> failure x
transIntent :: Intent -> Result
transIntent x = case x of
  In -> failure x
  Out -> failure x
  InOut -> failure x
  Ref -> failure x
  ConstIn -> failure x
  ConstRef -> failure x
transVDecl :: VDecl -> Result
transVDecl x = case x of
  Solo ident type_ -> failure x
  Init ident type_ rexp -> failure x
transCDecl :: CDecl -> Result
transCDecl x = case x of
  CDecl ident type_ rexp -> failure x
transType :: Type -> Result
transType x = case x of
  Type compound basic -> failure x
transCompound :: Compound -> Result
transCompound x = case x of
  Simple -> failure x
  Array compound rexp -> failure x
  Pointer compound -> failure x
transBasic :: Basic -> Result
transBasic x = case x of
  BBool -> failure x
  BChar -> failure x
  BInt -> failure x
  BReal -> failure x
  BString -> failure x
  BVoid -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block decls stms -> failure x
transStm :: Stm -> Result
transStm x = case x of
  StmBlock block -> failure x
  StmCall ident rexps -> failure x
  PredW pwrite rexp -> failure x
  Assign lexp assignop rexp -> failure x
  StmL lexp -> failure x
  If rexp stm -> failure x
  IfElse rexp stm1 stm2 -> failure x
  While rexp stm -> failure x
  DoWhile stm rexp -> failure x
  For ident range stm -> failure x
  JmpStm jump -> failure x
transJump :: Jump -> Result
transJump x = case x of
  Return -> failure x
  ReturnE rexp -> failure x
  Break -> failure x
  Continue -> failure x
transRange :: Range -> Result
transRange x = case x of
  Range rexp1 rexp2 -> failure x
transLExp :: LExp -> Result
transLExp x = case x of
  Deref lexp -> failure x
  Post lexp incdecop -> failure x
  Pre incdecop lexp -> failure x
  Access lexp rexp -> failure x
  Name ident -> failure x
transRExp :: RExp -> Result
transRExp x = case x of
  Or rexp1 rexp2 -> failure x
  And rexp1 rexp2 -> failure x
  Not rexp -> failure x
  Comp rexp1 compop rexp2 -> failure x
  Arith rexp1 arithop rexp2 -> failure x
  Sign signop rexp -> failure x
  RefE lexp -> failure x
  RLExp lexp -> failure x
  ArrList rexps -> failure x
  FCall ident rexps -> failure x
  PredR pread -> failure x
  Lit literal -> failure x
transPRead :: PRead -> Result
transPRead x = case x of
  ReadChar -> failure x
  ReadInt -> failure x
  ReadReal -> failure x
  ReadString -> failure x
transPWrite :: PWrite -> Result
transPWrite x = case x of
  WriteChar -> failure x
  WriteInt -> failure x
  WriteReal -> failure x
  WriteString -> failure x
transArithOp :: ArithOp -> Result
transArithOp x = case x of
  Add -> failure x
  Sub -> failure x
  Mul -> failure x
  Div -> failure x
  Mod -> failure x
  Pow -> failure x
transAssignOp :: AssignOp -> Result
transAssignOp x = case x of
  AssignEq -> failure x
  AssignAdd -> failure x
  AssignSub -> failure x
  AssignMul -> failure x
  AssignDiv -> failure x
  AssignMod -> failure x
  AssignPow -> failure x
transCompOp :: CompOp -> Result
transCompOp x = case x of
  Lt -> failure x
  Leq -> failure x
  Eq -> failure x
  Neq -> failure x
  Geq -> failure x
  Gt -> failure x
transIncDecOp :: IncDecOp -> Result
transIncDecOp x = case x of
  Inc -> failure x
  Dec -> failure x
transSignOp :: SignOp -> Result
transSignOp x = case x of
  Pos -> failure x
  Neg -> failure x
transLiteral :: Literal -> Result
transLiteral x = case x of
  LBool bool -> failure x
  LChar char -> failure x
  LInt integer -> failure x
  LReal double -> failure x
  LString string -> failure x
