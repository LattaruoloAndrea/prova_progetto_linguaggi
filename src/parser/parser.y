-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParChapel where
import AbsChapel
import LexChapel
import ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '%=' { PT _ (TS _ 4) }
  '&' { PT _ (TS _ 5) }
  '&&' { PT _ (TS _ 6) }
  '(' { PT _ (TS _ 7) }
  ')' { PT _ (TS _ 8) }
  '*' { PT _ (TS _ 9) }
  '**' { PT _ (TS _ 10) }
  '*=' { PT _ (TS _ 11) }
  '+' { PT _ (TS _ 12) }
  '++' { PT _ (TS _ 13) }
  '+=' { PT _ (TS _ 14) }
  ',' { PT _ (TS _ 15) }
  '-' { PT _ (TS _ 16) }
  '--' { PT _ (TS _ 17) }
  '-=' { PT _ (TS _ 18) }
  '..' { PT _ (TS _ 19) }
  '/' { PT _ (TS _ 20) }
  '/=' { PT _ (TS _ 21) }
  ':' { PT _ (TS _ 22) }
  ';' { PT _ (TS _ 23) }
  '<' { PT _ (TS _ 24) }
  '<=' { PT _ (TS _ 25) }
  '=' { PT _ (TS _ 26) }
  '==' { PT _ (TS _ 27) }
  '>' { PT _ (TS _ 28) }
  '>=' { PT _ (TS _ 29) }
  '[' { PT _ (TS _ 30) }
  ']' { PT _ (TS _ 31) }
  'bool' { PT _ (TS _ 32) }
  'break' { PT _ (TS _ 33) }
  'char' { PT _ (TS _ 34) }
  'const' { PT _ (TS _ 35) }
  'continue' { PT _ (TS _ 36) }
  'do' { PT _ (TS _ 37) }
  'else' { PT _ (TS _ 38) }
  'false' { PT _ (TS _ 39) }
  'for' { PT _ (TS _ 40) }
  'if' { PT _ (TS _ 41) }
  'in' { PT _ (TS _ 42) }
  'inout' { PT _ (TS _ 43) }
  'int' { PT _ (TS _ 44) }
  'out' { PT _ (TS _ 45) }
  'param' { PT _ (TS _ 46) }
  'proc' { PT _ (TS _ 47) }
  'readChar' { PT _ (TS _ 48) }
  'readFloat' { PT _ (TS _ 49) }
  'readInt' { PT _ (TS _ 50) }
  'readString' { PT _ (TS _ 51) }
  'real' { PT _ (TS _ 52) }
  'ref' { PT _ (TS _ 53) }
  'return' { PT _ (TS _ 54) }
  'string' { PT _ (TS _ 55) }
  'then' { PT _ (TS _ 56) }
  'true' { PT _ (TS _ 57) }
  'var' { PT _ (TS _ 58) }
  'void' { PT _ (TS _ 59) }
  'while' { PT _ (TS _ 60) }
  'writeChar' { PT _ (TS _ 61) }
  'writeFloat' { PT _ (TS _ 62) }
  'writeInt' { PT _ (TS _ 63) }
  'writeString' { PT _ (TS _ 64) }
  '{' { PT _ (TS _ 65) }
  '||' { PT _ (TS _ 66) }
  '}' { PT _ (TS _ 67) }
  L_ident  { PT _ (TV $$) }
  L_integ  { PT _ (TI $$) }
  L_charac { PT _ (TC $$) }
  L_doubl  { PT _ (TD $$) }
  L_quoted { PT _ (TL $$) }

%%

Ident   :: { Ident }
Ident    : L_ident  { Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read ( $1)) :: Integer }

Char    :: { Char }
Char     : L_charac { (read ( $1)) :: Char }

Double  :: { Double }
Double   : L_doubl  { (read ( $1)) :: Double }

String  :: { String }
String   : L_quoted {  $1 }

Program :: { Program }
Program : ListDecl { AbsChapel.Prog $1 }
Decl :: { Decl }
Decl : 'proc' Ident '(' ListForm ')' Intent ':' Type Block { AbsChapel.FDecl $2 $4 $6 $8 $9 }
     | 'var' ListVDecl ';' { AbsChapel.VList $2 }
     | 'param' ListCDecl ';' { AbsChapel.CList $2 }
Form :: { Form }
Form : Intent Ident ':' Type { AbsChapel.Form $1 $2 $4 }
Intent :: { Intent }
Intent : 'in' { AbsChapel.In }
       | 'out' { AbsChapel.Out }
       | 'inout' { AbsChapel.InOut }
       | 'ref' { AbsChapel.Ref }
       | 'const' 'in' { AbsChapel.ConstIn }
       | 'const' 'ref' { AbsChapel.ConstRef }
VDecl :: { VDecl }
VDecl : Ident ':' Type { AbsChapel.Solo $1 $3 }
      | Ident ':' Type '=' RExp { AbsChapel.Init $1 $3 $5 }
CDecl :: { CDecl }
CDecl : Ident ':' Type '=' RExp { AbsChapel.CDecl $1 $3 $5 }
Type :: { Type }
Type : Compound Basic { AbsChapel.Type $1 $2 }
Compound :: { Compound }
Compound : {- empty -} { AbsChapel.Simple }
         | Compound '[' RExp ']' { AbsChapel.Array $1 $3 }
         | Compound '*' { AbsChapel.Pointer $1 }
Basic :: { Basic }
Basic : 'bool' { AbsChapel.BBool }
      | 'char' { AbsChapel.BChar }
      | 'int' { AbsChapel.BInt }
      | 'real' { AbsChapel.BReal }
      | 'string' { AbsChapel.BString }
      | 'void' { AbsChapel.BVoid }
Block :: { Block }
Block : '{' ListDecl ListStm '}' { AbsChapel.Block $2 (reverse $3) }
Stm :: { Stm }
Stm : Block { AbsChapel.StmBlock $1 }
    | Ident '(' ListRExp ')' ';' { AbsChapel.StmCall $1 $3 }
    | PWrite '(' RExp ')' ';' { AbsChapel.PredW $1 $3 }
    | LExp AssignOp RExp ';' { AbsChapel.Assign $1 $2 $3 }
    | LExp ';' { AbsChapel.StmL $1 }
    | 'if' RExp 'then' Stm { AbsChapel.If $2 $4 }
    | 'if' RExp 'then' Stm 'else' Stm { AbsChapel.IfElse $2 $4 $6 }
    | 'while' RExp 'do' Stm { AbsChapel.While $2 $4 }
    | 'do' Stm 'while' RExp ';' { AbsChapel.DoWhile $2 $4 }
    | 'for' Ident 'in' Range 'do' Stm { AbsChapel.For $2 $4 $6 }
    | Jump ';' { AbsChapel.JmpStm $1 }
Jump :: { Jump }
Jump : 'return' { AbsChapel.Return }
     | 'return' RExp { AbsChapel.ReturnE $2 }
     | 'break' { AbsChapel.Break }
     | 'continue' { AbsChapel.Continue }
Range :: { Range }
Range : '{' Integer '..' Integer '}' { AbsChapel.Range $2 $4 }
LExp :: { LExp }
LExp : '*' LExp { AbsChapel.Deref $2 }
     | LExp IncDecOp { AbsChapel.Post $1 $2 }
     | IncDecOp LExp { AbsChapel.Pre $1 $2 }
     | LExp '[' RExp ']' { AbsChapel.Access $1 $3 }
     | Ident { AbsChapel.Name $1 }
RExp :: { RExp }
RExp : RExp '||' RExp { AbsChapel.Or $1 $3 }
     | RExp '&&' RExp { AbsChapel.And $1 $3 }
     | '!' RExp { AbsChapel.Not $2 }
     | RExp CompOp RExp { AbsChapel.Comp $1 $2 $3 }
     | RExp ArithOp RExp { AbsChapel.Arith $1 $2 $3 }
     | SignOp RExp { AbsChapel.Sign $1 $2 }
     | '&' LExp { AbsChapel.RefE $2 }
     | LExp { AbsChapel.RLExp $1 }
     | '[' ListRExp ']' { AbsChapel.ArrList $2 }
     | Ident '(' ListRExp ')' { AbsChapel.FCall $1 $3 }
     | PRead '(' ')' { AbsChapel.PredR $1 }
     | Literal { AbsChapel.Lit $1 }
PRead :: { PRead }
PRead : 'readChar' { AbsChapel.ReadChar }
      | 'readInt' { AbsChapel.ReadInt }
      | 'readFloat' { AbsChapel.ReadFloat }
      | 'readString' { AbsChapel.ReadString }
PWrite :: { PWrite }
PWrite : 'writeChar' { AbsChapel.WriteChar }
       | 'writeInt' { AbsChapel.WriteInt }
       | 'writeFloat' { AbsChapel.WriteFloat }
       | 'writeString' { AbsChapel.WriteString }
ArithOp :: { ArithOp }
ArithOp : '+' { AbsChapel.Add }
        | '-' { AbsChapel.Sub }
        | '*' { AbsChapel.Mul }
        | '/' { AbsChapel.Div }
        | '%' { AbsChapel.Mod }
        | '**' { AbsChapel.Pow }
AssignOp :: { AssignOp }
AssignOp : '=' { AbsChapel.AssignEq }
         | '+=' { AbsChapel.AssignAdd }
         | '-=' { AbsChapel.AssignSub }
         | '*=' { AbsChapel.AssignMul }
         | '/=' { AbsChapel.AssignDiv }
         | '%=' { AbsChapel.AssignMod }
CompOp :: { CompOp }
CompOp : '<' { AbsChapel.Lt }
       | '<=' { AbsChapel.Leq }
       | '==' { AbsChapel.Eq }
       | '!=' { AbsChapel.Neq }
       | '>=' { AbsChapel.Geq }
       | '>' { AbsChapel.Gt }
IncDecOp :: { IncDecOp }
IncDecOp : '++' { AbsChapel.Inc } | '--' { AbsChapel.Dec }
SignOp :: { SignOp }
SignOp : '+' { AbsChapel.Pos } | '-' { AbsChapel.Neg }
Literal :: { Literal }
Literal : Boolean { AbsChapel.LBool $1 }
        | Char { AbsChapel.LChar $1 }
        | Integer { AbsChapel.LInt $1 }
        | Double { AbsChapel.LReal $1 }
        | String { AbsChapel.LString $1 }
Boolean :: { Boolean }
Boolean : 'false' { AbsChapel.BFalse } | 'true' { AbsChapel.BTrue }
ListForm :: { [Form] }
ListForm : {- empty -} { [] }
         | Form { (:[]) $1 }
         | Form ',' ListForm { (:) $1 $3 }
ListDecl :: { [Decl] }
ListDecl : Decl { (:[]) $1 } | Decl ListDecl { (:) $1 $2 }
ListVDecl :: { [VDecl] }
ListVDecl : VDecl { (:[]) $1 } | VDecl ',' ListVDecl { (:) $1 $3 }
ListCDecl :: { [CDecl] }
ListCDecl : CDecl { (:[]) $1 } | CDecl ',' ListCDecl { (:) $1 $3 }
ListStm :: { [Stm] }
ListStm : {- empty -} { [] } | ListStm Stm { flip (:) $1 $2 }
ListRExp :: { [RExp] }
ListRExp : {- empty -} { [] }
         | RExp { (:[]) $1 }
         | RExp ',' ListRExp { (:) $1 $3 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}

