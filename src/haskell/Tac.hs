module Tac where

import AbsChapel (Loc, line, column, Literal)

data Addr
    = AName String Loc
    | ALit  Literal
    | ATemp String
    deriving (Read, Eq, Ord)

data Label = Label String
    deriving (Read, Eq, Ord)

data LAddr
    = A Addr
    | Arr { base, offset :: Addr }
    | RefTo Addr
    deriving (Read, Eq, Ord)

data Static = SStr LAddr String
    deriving (Read, Eq, Ord)

getAddr :: LAddr -> Addr
getAddr l = case l of
    A a     -> a
    Arr a _ -> a
    RefTo a -> a

getName :: LAddr -> String
getName (A (ATemp name)) = name


data Over = B | C | I | R | S | P
    deriving (Read, Eq, Ord)

data CompOp
    = Lt    Over
    | Leq   Over
    | Eq    Over
    | Neq   Over
    | Geq   Over
    | Gt    Over
    deriving (Read, Eq, Ord)

data BinOp
    = Rel CompOp
    | Add Over
    | Sub Over
    | Mul Over
    | Div Over
    | Mod
    | Pow Over
    | Or
    | And
    deriving (Read, Eq, Ord)

data UnOp
    = Neg    Over
    | Not
    | Coerce Over Over

data TAC
    = Bin LAddr LAddr BinOp LAddr Over      -- x = y bop z
    | Un  LAddr UnOp  LAddr Over            -- x = uop y
    | Nil LAddr LAddr Over                  -- x = y
    | Goto Label                            -- goto label
    | Lab Label                             -- label: ...
    | If LAddr Label                        -- if x goto label
    | IfFalse LAddr Label                   -- ifFalse x goto label
    | IfRel LAddr CompOp LAddr Label        -- if x rel y goto label
    | Ref LAddr LAddr                       -- x = &y
    | Par LAddr                             -- param x
    | Call LAddr Int                        -- call proc, n
    | FCall LAddr LAddr Int                 -- x = fcall fun, n
    | Return                                -- return
    | ReturnE LAddr                         -- return x
    | Exit                                  -- exit
    | Stat Static                           -- static data
    | Comment String                        -- # some comment..


class Overloaded a where
    overT :: a -> Over

instance Overloaded CompOp where
    overT op = case op of
        Lt  o -> o
        Leq o -> o
        Eq  o -> o
        Neq o -> o
        Geq o -> o
        Gt  o -> o

instance Overloaded BinOp where
    overT op = case op of
        Rel cop -> overT cop
        Add o   -> o
        Sub o   -> o
        Mul o   -> o
        Div o   -> o
        Mod     -> I
        Pow o   -> o
        Or      -> B
        And     -> B

instance Overloaded UnOp where
    overT op = case op of
        Neg o       -> o
        Not         -> B
        Coerce f t  -> t





instance Show Addr where
    show a = case a of
        AName s loc -> s ++ extra
            where extra = if line loc < 0
                            then "$predefined"
                            else "@" ++ (show $ line loc) ++ "," ++ (show $ column loc)
        ALit lit    -> show lit
        ATemp s     -> s


instance Show Label where
    show (Label s) = s


instance Show LAddr where
    show a = case a of
        A a     -> show a
        Arr b o -> (show b) ++ "[" ++ (show o) ++ "]"
        RefTo a -> "*" ++ (show a)


instance Show Static where
    show (SStr addr val) = (show addr) ++ "\t\t" ++ (show val)


instance Show Over where
    show o = case o of
        B -> "bool"
        C -> "char"
        I -> "int"
        R -> "real"
        S -> "string"
        P -> "addr"


instance Show CompOp where
    show op = case op of
        Lt  o -> "less_than_" ++ (show o)
        Leq o -> "less_equal_" ++ (show o)
        Eq  o -> "equal_" ++ (show o)
        Neq o -> "not_equal_" ++ (show o)
        Geq o -> "greater_equal_" ++ (show o)
        Gt  o -> "greater_than_" ++ (show o)

instance Show BinOp where
    show op = case op of
        Rel cop -> show cop
        Add o   -> "add_" ++ (show o)
        Sub o   -> "sub_" ++ (show o)
        Mul o   -> "mul_" ++ (show o)
        Div o   -> "div_" ++ (show o)
        Mod     -> "mod_int"
        Pow o   -> "pow_" ++ (show o)
        Or      -> "||"
        And     -> "&&"

instance Show UnOp where
    show op = case op of
        Neg o       -> "neg_" ++ (show o)
        Not         -> "not"
        Coerce f t  -> "convert_" ++ (show f) ++ "_to_" ++ (show t)


instance Show TAC where
    show instr = (++) tt $ case instr of
        Bin x y bop z o     -> (show x) ++ " :=" ++ (show o) ++ " " ++ (show y) ++ " " ++ (show bop) ++ " " ++ (show z)
        Un  x uop y o       -> (show x) ++ " :=" ++ (show o) ++ " " ++ (show uop) ++ " " ++ (show y)
        Nil x y o           -> (show x) ++ " :=" ++ (show o) ++ " " ++ (show y)
        Goto lab            -> "goto " ++ (show lab)
        Lab lab             -> "\n" ++ (show lab) ++ ":"
        If x lab            -> "if " ++ (show x) ++ " goto " ++ (show lab)
        IfFalse x lab       -> "ifFalse " ++ (show x) ++ " goto " ++ (show lab)
        IfRel x rel y lab   -> "if " ++ (show x) ++ " " ++ (show rel) ++ " " ++ (show y) ++ " goto " ++ (show lab)
        Ref x y             -> (show x) ++ " :=addr &" ++ (show y)
        Par x               -> "param " ++ (show x)
        Call f n            -> "call " ++ (show f) ++ " " ++ (show n)
        FCall x f n         -> (show x) ++ " := " ++ "fcall " ++ (show f) ++ " " ++ (show n)
        Return              -> "return"
        ReturnE x           -> "return " ++ (show x)
        Exit                -> "exit"
        Stat s              -> show s
        Comment s           -> "\n\t# " ++ s
        where
            tt = if isLab instr then "" else "\t"
            isLab (Lab _) = True
            isLab _       = False