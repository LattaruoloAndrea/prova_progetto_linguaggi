module Locatable where

import AbsChapel

class Locatable a where -- Minimal complete definition => locOf
    locOf :: a -> Loc
    linOf :: a -> Int
    colOf :: a -> Int
    linColOf :: a-> (Int, Int)

    linOf = line . locOf
    colOf = column . locOf
    linColOf x = (linOf x, colOf x)  -- linColOf = (,) <$> linOf <*> colOf
                                     -- for the braves with point-free style

instance Locatable Ident where
    locOf = idLoc

instance Locatable Decl where
    locOf x = case x of
        FDecl id _ _ _ _ -> locOf id
        VList vs         -> locOf $ head vs
        CList cs         -> locOf $ head cs

instance Locatable VDecl where
    locOf x = case x of
        Solo id _   -> locOf id
        Init id _ _ -> locOf id

instance Locatable CDecl where
    locOf (CDecl id _ _) = locOf id

instance Locatable Form where
    locOf (Form _ id _) = locOf id

instance Locatable Block where
    locOf = bLoc

instance Locatable Jump where
    locOf = jmpLoc

instance Locatable Range where
    locOf = rngLoc

instance Locatable ArithOp where
    locOf = arLoc

instance Locatable AssignOp where
    locOf = asLoc
    
instance Locatable CompOp where
    locOf = coLoc

instance Locatable SignOp where
    locOf = sgnLoc

instance Locatable LExp where
    locOf x = case x of
        Deref lexp      -> locOf x
        Access lexp _   -> locOf lexp
        Name id         -> locOf id

instance Locatable PRead where
    locOf = prLoc

instance Locatable PWrite where
    locOf = pwLoc

instance Locatable RExp where
    locOf = reLoc

instance Locatable Stm where
    locOf x = case x of
        StmBlock b      -> locOf b
        StmCall id _    -> locOf id
        PredW pw _      -> locOf pw
        Assign _ op _   -> locOf op
        StmL lexp       -> locOf lexp
        If rexp _       -> locOf rexp
        IfElse rexp _ _ -> locOf rexp
        While rexp _    -> locOf rexp
        DoWhile _ rexp  -> locOf rexp
        For id _ _      -> locOf id
        JmpStm jmp      -> locOf jmp