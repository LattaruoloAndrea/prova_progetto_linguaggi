module Locatable where

import AbsChapel

-- Class to retrieve the location information (AbsChapel.Loc) from a

class Locatable a where -- Minimal complete definition => locOf
    locOf :: a -> Loc
    linOf :: a -> Int
    colOf :: a -> Int
    linColOf :: a-> (Int, Int)

    linOf = line . locOf
    colOf = column . locOf
    linColOf x = (linOf x, colOf x)  -- linColOf = (,) <$> linOf <*> colOf
                                     -- for the braves with point-free style


-- INSTANCES ///////////////////////////////////////////////////////////////
--
-- Most of them are based on fieldname in AbsChapel

instance Locatable Ident where
    locOf = idLoc

instance Locatable (Decl t) where
    locOf x = case x of
        FDecl id _ _ _ _ -> locOf id
        VList vs         -> locOf $ head vs -- list should be non-empty
        CList cs         -> locOf $ head cs -- list should be non-empty

instance Locatable (VDecl t) where
    locOf x = case x of
        Solo id _   -> locOf id
        Init id _ _ -> locOf id

instance Locatable (CDecl t) where
    locOf (CDecl id _ _) = locOf id

instance Locatable (Form t) where
    locOf (Form _ id _) = locOf id

instance Locatable (Block t) where
    locOf = bLoc

instance Locatable (Jump t) where
    locOf = jmpLoc

instance Locatable (Range t) where
    locOf = rngLoc

instance Locatable (AssignOp t) where
    locOf = asLoc

instance Locatable (LExp t) where
    locOf x = case x of
        Deref lexp _    -> locOf lexp
        Access lexp _ _ -> locOf lexp
        Name id _       -> locOf id

instance Locatable (RExp t) where
    locOf = reLoc

instance Locatable (Stm t) where
    locOf x = case x of
        StmBlock b      -> locOf b
        StmCall id _    -> locOf id
        Assign _ op _   -> locOf op
        StmL lexp       -> locOf lexp
        If rexp _       -> locOf rexp
        IfElse rexp _ _ -> locOf rexp
        While rexp _    -> locOf rexp
        DoWhile _ rexp  -> locOf rexp
        For id _ _      -> locOf id
        JmpStm jmp      -> locOf jmp