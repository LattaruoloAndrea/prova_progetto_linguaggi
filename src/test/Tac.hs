module Tac where

import AbsChapel (Loc, Literal)

data Addr
    = AName String Loc
    | ALit  Literal
    | ATemp String
    deriving (Read, Show, Eq, Ord)

data Label = Label String
    deriving (Read, Show, Eq, Ord)

data LAddr
    = A Addr
    | Arr { base, offset :: Addr }
    | RefTo Addr
    deriving (Read, Show, Eq, Ord)

data Over = B | C | I | R | S
    deriving (Read, Show, Eq, Ord)

data CompOp
    = Lt
    | Leq
    | Eq
    | Neq
    | Geq
    | Gt
    deriving (Read, Show, Eq, Ord)

data BinOp
    = Rel CompOp Over
    | Add Over
    | Sub Over
    | Mul Over
    | Div Over
    | Mod
    | Pow Over
    | Or
    | And
    deriving (Read, Show, Eq, Ord)

data UnOp
    = Neg    Over
    | Not
    | Coerce Over
    deriving (Show)

data TAC
    = Bin LAddr LAddr BinOp LAddr           -- x = y bop z
    | Un  LAddr UnOp  LAddr                 -- x = uop y
    | Nil LAddr LAddr                       -- x = y
    | Goto Label                            -- goto label
    | If LAddr Label                        -- if x goto label
    | IfFalse LAddr Label                   -- ifFalse x goto label
    | IfRel LAddr CompOp LAddr Label        -- if x rel y goto label
    | CopyL LAddr LAddr LAddr               -- x[y] = z
    | CopyR LAddr LAddr LAddr               -- x = y[z]
    | Ref LAddr LAddr                       -- x = &y
    | DerefL LAddr LAddr                    -- *x = y
    | DerefR LAddr LAddr                    -- x = *y
    | Par LAddr                             -- param x
    | Call LAddr Int                        -- call proc, n
    | FCall LAddr LAddr Int                 -- x = fcall fun, n
    | Return                                -- return
    | ReturnE LAddr                         -- return x
    deriving (Show)