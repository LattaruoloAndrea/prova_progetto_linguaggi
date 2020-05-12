-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module AbsGarpezpp where

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)

newtype Boolean = Boolean String
  deriving (Eq, Ord, Show, Read)

data Program = Prog [FDecl]
  deriving (Eq, Ord, Show, Read)

data FDecl = FDecl RType Ident [Param] Block
  deriving (Eq, Ord, Show, Read)

data Param = Param Type PassBy Ident
  deriving (Eq, Ord, Show, Read)

data PassBy = PassBy_ | PassBy1
  deriving (Eq, Ord, Show, Read)

data DList = VList Type [VDecl] | CList [CDecl]
  deriving (Eq, Ord, Show, Read)

data VDecl = VSolo Ident | VInit Ident RExp
  deriving (Eq, Ord, Show, Read)

data CDecl = CDecl Ident RExp
  deriving (Eq, Ord, Show, Read)

data Type = Type Basic Compound
  deriving (Eq, Ord, Show, Read)

data Compound = Simple | Array RExp Compound | Pointer Compound
  deriving (Eq, Ord, Show, Read)

data Basic
    = Basic_bool | Basic_char | Basic_int | Basic_float | Basic_string
  deriving (Eq, Ord, Show, Read)

data RType = RTypeBasic Basic | RType1 Type
  deriving (Eq, Ord, Show, Read)

data Block = Block [DList] [Stm]
  deriving (Eq, Ord, Show, Read)

data Stm
    = StmBlock Block
    | StmCall Ident [RExp]
    | PredW PWrite RExp
    | Assign LExp AssignOp RExp
    | StmL LExp
    | If RExp Stm
    | IfElse RExp Stm Stm
    | While RExp Stm
    | DoWhile Stm RExp
    | For Ident RExp Dir RExp Stm
    | JmpStm Jump
  deriving (Eq, Ord, Show, Read)

data Dir = UpTo | DownTo
  deriving (Eq, Ord, Show, Read)

data Jump = Jump_return | Jump1 RExp | Jump_break | Jump_continue
  deriving (Eq, Ord, Show, Read)

data LExp
    = Deref LExp
    | Post LExp IncDecOp
    | Pre IncDecOp LExp
    | Access LExp RExp
    | Name Ident
  deriving (Eq, Ord, Show, Read)

data RExp
    = Or RExp RExp
    | And RExp RExp
    | Not RExp
    | Comp RExp CompOp RExp
    | Add RExp RExp
    | Sub RExp RExp
    | Mul RExp RExp
    | Div RExp RExp
    | Rem RExp RExp
    | Pow RExp RExp
    | Sign SignOp RExp
    | Ref LExp
    | RLExp LExp
    | FCall Ident [RExp]
    | PredR PRead
    | Lit Literal
  deriving (Eq, Ord, Show, Read)

data PRead
    = PRead_readChar
    | PRead_readInt
    | PRead_readFloat
    | PRead_readString
  deriving (Eq, Ord, Show, Read)

data PWrite
    = PWrite_writeChar
    | PWrite_writeInt
    | PWrite_writeFloat
    | PWrite_writeString
  deriving (Eq, Ord, Show, Read)

data AssignOp
    = AssignOp1
    | AssignOp2
    | AssignOp3
    | AssignOp4
    | AssignOp5
    | AssignOp6
    | AssignOp7
    | AssignOp8
    | AssignOp9
  deriving (Eq, Ord, Show, Read)

data CompOp
    = CompOp1 | CompOp2 | CompOp3 | CompOp4 | CompOp5 | CompOp6
  deriving (Eq, Ord, Show, Read)

data IncDecOp = IncDecOp1 | IncDecOp2
  deriving (Eq, Ord, Show, Read)

data SignOp = SignOp1 | SignOp2
  deriving (Eq, Ord, Show, Read)

data Literal
    = LiteralBoolean Boolean
    | LiteralChar Char
    | LiteralInteger Integer
    | LiteralDouble Double
    | LiteralString String
  deriving (Eq, Ord, Show, Read)

