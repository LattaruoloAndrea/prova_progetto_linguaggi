{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser where
import AbstractSyntaxTree as AST
import LexGarpezpp
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
  '*=' { PT _ (TS _ 10) }
  '+' { PT _ (TS _ 11) }
  '++' { PT _ (TS _ 12) }
  '+=' { PT _ (TS _ 13) }
  ',' { PT _ (TS _ 14) }
  '-' { PT _ (TS _ 15) }
  '--' { PT _ (TS _ 16) }
  '-=' { PT _ (TS _ 17) }
  '/' { PT _ (TS _ 18) }
  '/=' { PT _ (TS _ 19) }
  ';' { PT _ (TS _ 20) }
  '<' { PT _ (TS _ 21) }
  '<=' { PT _ (TS _ 22) }
  '=' { PT _ (TS _ 23) }
  '==' { PT _ (TS _ 24) }
  '>' { PT _ (TS _ 25) }
  '>=' { PT _ (TS _ 26) }
  '[' { PT _ (TS _ 27) }
  ']' { PT _ (TS _ 28) }
  '^' { PT _ (TS _ 29) }
  'bool' { PT _ (TS _ 30) }
  'break' { PT _ (TS _ 31) }
  'char' { PT _ (TS _ 32) }
  'const' { PT _ (TS _ 33) }
  'continue' { PT _ (TS _ 34) }
  'do' { PT _ (TS _ 35) }
  'downto' { PT _ (TS _ 36) }
  'else' { PT _ (TS _ 37) }
  'false' { PT _ (TS _ 38) }
  'float' { PT _ (TS _ 39) }
  'for' { PT _ (TS _ 40) }
  'if' { PT _ (TS _ 41) }
  'int' { PT _ (TS _ 42) }
  'name' { PT _ (TS _ 43) }
  'readChar' { PT _ (TS _ 44) }
  'readFloat' { PT _ (TS _ 45) }
  'readInt' { PT _ (TS _ 46) }
  'readString' { PT _ (TS _ 47) }
  'ref' { PT _ (TS _ 48) }
  'res' { PT _ (TS _ 49) }
  'return' { PT _ (TS _ 50) }
  'string' { PT _ (TS _ 51) }
  'true' { PT _ (TS _ 52) }
  'upto' { PT _ (TS _ 53) }
  'val' { PT _ (TS _ 54) }
  'valres' { PT _ (TS _ 55) }
  'void' { PT _ (TS _ 56) }
  'while' { PT _ (TS _ 57) }
  'writeChar' { PT _ (TS _ 58) }
  'writeFloat' { PT _ (TS _ 59) }
  'writeInt' { PT _ (TS _ 60) }
  'writeString' { PT _ (TS _ 61) }
  '{' { PT _ (TS _ 62) }
  '||' { PT _ (TS _ 63) }
  '}' { PT _ (TS _ 64) }
  L_ident  { PT _ (TV $$) }
  L_charac { PT _ (TC $$) }
  L_integ  { PT _ (TI $$) }
  L_doubl  { PT _ (TD $$) }
  L_quoted { PT _ (TL $$) }

%left '||'
%left '&&'
%nonassoc '<' '<=' '==' '!=' '>=' '>'
%left '+' '-'
%left '*' '/'
%left '%'
%right '!' SIGN
%right '^'
%nonassoc '++' '--'
%right PREINCDEC
%right '['
%nonassoc ']'

%%

Ident   :: { Ident }
Ident    : L_ident  { AST.Ident (prToken $1) (tokenLoc $1) }

Char    :: { (Char, Loc) }
Char     : L_charac { ( (read ( $1)) :: Char, tokenLoc $1 ) }

Integer :: { (Integer, Loc) }
Integer  : L_integ  { ( (read ( $1)) :: Integer, tokenLoc $1 ) }

Double  :: { (Double, Loc) }
Double   : L_doubl  { ( (read ( $1)) :: Double, tokenLoc $1 ) }

String  :: { (String, Loc) }
String   : L_quoted {  ( prToken $1, tokenLoc $1 ) }

Program :: { Program }
Program : ListFDecl { AST.Prog [] $1 }

FDecl :: { FDecl }
FDecl : RType Ident '(' ListParam ')' Block { AST.FDecl $1 $2 $4 $6 ($2^.loc) }

Param :: { Param }
Param : Type PassBy Ident { AST.Param $1 $2 $3 ($3^.loc) }

PassBy :: { PassBy }
PassBy : 'val' { AST.PassVal }
       | 'ref' { AST.PassRef }
       | 'name' { AST.PassName }
       | 'res' { AST.PassRes }
       | 'valres' { AST.PassValRes }
       | 'const' { AST.PassConst }

DList :: { DList }
DList : Type ListVDecl ';' { let l = if null $2 then AST.NoLoc else (loc . head) $2 in AST.VList $1 $2 l }
      | 'const' ListCDecl ';' { let l = if null $2 then AST.NoLoc else (loc . head) $2 in AST.CList $2 l }

VDecl :: { VDecl }
VDecl : Ident { AST.VSolo $1 ($1^.loc) }
      | Ident '=' RExp { AST.VInit $1 $3 ($1^.loc) }

CDecl :: { CDecl }
CDecl : Ident '=' RExp { AST.CDecl $1 $3 ($1^.loc) }

Type :: { Type }
Type : Basic Compound { AST.Type $1 $2 }

Compound :: { Compound }
Compound : {- empty -} { AST.Simple }
         | Compound '[' RExp ']' { AST.Array $1 $3 }
         | Compound '*' { AST.Pointer $1 }

Basic :: { Basic }
Basic : 'bool' { AST.BBool }
      | 'char' { AST.BChar }
      | 'int' { AST.BInt }
      | 'float' { AST.BFloat }
      | 'string' { AST.BString }

RType :: { RType }
RType : 'void' { AST.RVoid }
      | Basic { AST.RBasic $1 }
      | Type '&' { AST.RRef $1 }

Block :: { Block }
Block : '{' ListDList ListStm '}' { AST.Block (reverse $2) (reverse $3) (tokenLoc $1) }

Stm :: { Stm }
Stm : {- Stm ';' { $1 } -- Regola per "mangiare" i ;
	| -} Block { AST.StmBlock $1 ($1^.loc) }
    | Ident '(' ListRExp ')' ';' { AST.StmCall $1 $3 ($1.^loc) }
    | 'writeChar' '(' RExp ')' ';' { AST.WriteChar $3 (tokenLoc $1) }
    | 'writeInt' '(' RExp ')' ';' { AST.WriteInt $3 (tokenLoc $1) }
    | 'writeFloat' '(' RExp ')' ';' { AST.WriteFloat $3 (tokenLoc $1) }
    | 'writeString' '(' RExp ')' ';' { AST.WriteString $3 (tokenLoc $1) }
    | LExp '=' RExp ';' { AST.AssignEq $1 $3 (tokenLoc $2) }
    | LExp '+=' RExp ';' { AST.AssignAdd $1 $3 (tokenLoc $2) }
    | LExp '-=' RExp ';' { AST.AssignSub $1 $3 (tokenLoc $2) }
    | LExp '*=' RExp ';' { AST.AssignMul $1 $3 (tokenLoc $2) }
    | LExp '/=' RExp ';' { AST.AssignDiv $1 $3 (tokenLoc $2) }
    | LExp '%=' RExp ';' { AST.AssignMod $1 $3 (tokenLoc $2) }
    | LExp ';' { AST.StmL $1 ($1^.loc) }
    | 'if' '(' RExp ')' Stm { AST.If $3 $5 (tokenLoc $1) }
    | 'if' '(' RExp ')' Stm 'else' Stm { AST.IfElse $3 $5 $7 (tokenLoc $1) }
    | 'while' '(' RExp ')' Stm { AST.While $3 $5 (tokenLoc $1) }
    | 'do' Stm 'while' '(' RExp ')' ';' { AST.DoWhile $2 $5 (tokenLoc $1) }
    | 'for' '(' Ident '=' RExp 'upto' RExp ')' Stm { AST.ForUp $3 $5 $6 $7 $9 (tokenLoc $1) }
    | 'for' '(' Ident '=' RExp 'downto' RExp ')' Stm { AST.ForDown $3 $5 $6 $7 $9 (tokenLoc $1) }
    | 'return' ';' { AST.Return $ tokenLoc $1 }
    | 'return' RExp ';' { AST.ReturnE $2 (tokenLoc $1) }
    | 'break' ';' { AST.Break $ tokenLoc $1 }
    | 'continue' ';' { AST.Continue $ tokenLoc $1 }

LExp :: { LExp }
LExp : '*' LExp { AST.Deref $2 (tokenLoc $1) }
	| LExp '++' { AST.PostInc $1 (tokenLoc $2) }
	| LExp '--' { AST.PostDec $1 (tokenLoc $2) }
	| '++' LExp %prec PREINCDEC { AST.PreInc $2 (tokenLoc $1) }
	| '--' LExp %prec PREINCDEC { AST.PreDec $2 (tokenLoc $1) }
	| LExp '[' RExp ']' { AST.Access $1 $3 ($1^.loc) }
	| Ident { AST.Name $1 ($1^.loc) }
	| '(' LExp ')' { $2 }

RExp :: { RExp }
RExp : RExp '||' RExp { AST.Or $1 $3 (tokenLoc $2) }
	| RExp '&&' RExp { AST.And $1 $3 (tokenLoc $2) }
	| '!' RExp { AST.Not $2 (tokenLoc $1) }
	| RExp '<' RExp { AST.Lt $1 $3 (tokenLoc $2) }
	| RExp '<=' RExp { AST.Le $1 $3 (tokenLoc $2) }
	| RExp '==' RExp { AST.Eq $1 $3 (tokenLoc $2) }
	| RExp '!=' RExp { AST.Neq $1 $3 (tokenLoc $2) }
	| RExp '>=' RExp { AST.Ge $1 $3 (tokenLoc $2) }
	| RExp '>' RExp { AST.Gt $1 $3 (tokenLoc $2) }
	| RExp '+' RExp { AST.Add $1 $3 (tokenLoc $2) }
	| RExp '-' RExp { AST.Sub $1 $3 (tokenLoc $2) }
	| RExp '*' RExp { AST.Mul $1 $3 (tokenLoc $2) }
	| RExp '/' RExp { AST.Div $1 $3 (tokenLoc $2) }
	| RExp '%' RExp { AST.Rem $1 $3 (tokenLoc $2) }
	| RExp '^' RExp { AST.Pow $1 $3 (tokenLoc $2) }
	| '+' RExp %prec SIGN { AST.Plus $2 (tokenLoc $1) }
	| '-' RExp %prec SIGN { AST.Minus $2 (tokenLoc $1) }
	| '&' LExp { AST.Ref $2 (tokenLoc $1) }
	| LExp { AST.RLExp $1 ($1^.loc) }
	| '[' ListRExp ']' { AST.ArrList $2 (tokenLoc $1) }
    | Ident '(' ListRExp ')' { AST.FCall $1 $3 ($1^.loc) }
	| 'readChar' '(' ')' { AST.ReadChar $ tokenLoc $1 }
	| 'readInt' '(' ')' { AST.ReadInt $ tokenLoc $1 }
	| 'readFloat' '(' ')' { AST.ReadFloat $ tokenLoc $1 }
	| 'readString' '(' ')' { AST.ReadString $ tokenLoc $1 }
	| 'false' { AST.LFalse $ tokenLoc $1 }
	| 'true' { AST.LTrue $ tokenLoc $1 }
	| Char { AST.LChar (fst $1) (snd $1) }
	| Integer { AST.LInt (fst $1) (snd $1) }
	| Double { AST.LFloat (fst $1) (snd $1) }
	| String { AST.LString (fst $1) (snd $1) }
	| '(' RExp ')' { $1 }

ListParam :: { [Param] }
ListParam : {- empty -} { [] }
          | Param { (:[]) $1 }
          | Param ',' ListParam { (:) $1 $3 }

ListFDecl :: { [FDecl] }
ListFDecl : FDecl { (:[]) $1 }
		  | FDecl ListFDecl { (:) $1 $2 }

ListVDecl :: { [VDecl] }
ListVDecl : VDecl { (:[]) $1 }
		  | VDecl ',' ListVDecl { (:) $1 $3 }

ListCDecl :: { [CDecl] }
ListCDecl : CDecl { (:[]) $1 }
		  | CDecl ',' ListCDecl { (:) $1 $3 }

ListDList :: { [DList] }
ListDList : {- empty -} { [] }
		  | ListDList DList { flip (:) $1 $2 }

ListStm :: { [Stm] }
ListStm : {- empty -} { [] }
		| ListStm Stm { flip (:) $1 $2 }

ListRExp :: { [RExp] }
ListRExp : RExp { (:[]) $1 }
		| RExp ',' ListRExp { (:) $1 $3 }


{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

tokenLoc :: Token -> Loc
tokenLoc t =
	let	(l, c) = tokenLineCol t
	in	AST.Loc l c

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}

