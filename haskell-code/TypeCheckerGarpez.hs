module TypeChecker where

import qualified Data.Map.Strict as M
import AbsGarpez
import ErrM
import Control.Monad (guard, liftM2, join)
import Data.Maybe
import Data.List

-- // DATA TYPES //////////////////////////////////////////////////////////
type Ident = String
type Env = [Context]
type Context = M.Map Ident EnvEntry
data EnvEntry
   = Variable Loc TCType
   | Function Loc [Arg] TCType
   | Constant Loc TCType
   deriving (Show)

data Arg = Arg Loc PassBy TCType Ident
   deriving (Show)

data Loc = Loc {line, column :: Int}
   deriving (Show)

data TCType
   = TCBool
   | TCChar
   | TCInt
   | TCFloat
   | TCString
   | TCVoid
   deriving (Show, Eq, Ord)


-- TYPE ALIASES //////////////////////////////////////////////////////////

-- bool = (TCBool)
-- char = (TCChar)
-- int  = (TCInt)
-- float = (TCFloat)
-- string = (TCString)


-- CLASS DEFINITIONS //////////////////////////////////////////////////////
class Identifiable a where
   identOf :: a -> Ident

class Localizable a where
   locOf :: a -> Loc

class PartialOrd a where
   (<=.) :: a -> a -> Err Bool -- Minimal complete definition
   (==.) :: a -> a -> Err Bool
   (/=.) :: a -> a -> Err Bool
   (>=.) :: a -> a -> Err Bool
   (<.) :: a -> a -> Err Bool
   (>.) :: a -> a -> Err Bool


   x ==. y = (&&) <$> x <=. y <*> y <=. x
   x /=. y = not <$> x ==. y
   x >=. y = y <=. x
   x <. y  = not <$> x >=. y
   x >. y  = not <$> x <=. y


class TCTypeable a where
   tcTypeOf :: a -> TCType

-- INSTANCES //////////////////////////////////////////////////////////////
instance Identifiable Id where
   identOf (Id (_, id)) = id

instance Localizable Id where
   locOf (Id ((l, c), _)) = Loc l c

instance Localizable EnvEntry where
   locOf (Variable loc _) = loc
   locOf (Function loc _ _) = loc
   locOf (Constant loc _) = loc

instance PartialOrd TCType where
   x <=. y = case (x, y) of
      (_, TCBool) -> guard (x==y) >> Ok True
      (_, TCString)-> guard (x==y) >> Ok True
      _ -> return (x <= y)


instance TCTypeable RPredefined where
   tcTypeOf x = case x of
      (RPredefinedRChar _) -> TCChar
      (RPredefinedRInt _)  -> TCInt
      (RPredefinedRFloat _) -> TCFloat
      (RPredefinedRString _) -> TCString

instance TCTypeable Literal where
   tcTypeOf x = case x of
      (LiteralPBool _) -> TCBool
      (LiteralPChar _) -> TCChar
      (LiteralPInt _) -> TCInt
      (LiteralPFloat _) -> TCFloat
      (LiteralPString _) -> TCString


instance TCTypeable SimpleType where
   tcTypeOf x = case x of
      SimpleType_bool -> TCBool
      SimpleType_char -> TCChar
      SimpleType_int  -> TCInt
      SimpleType_float -> TCFloat
      SimpleType_string -> TCString

instance TCTypeable Type where
   tcTypeOf (SType x) = tcTypeOf x
   tcTypeOf _ = ()


-- ////////////////////////////////////////////////////////////////////////

updateVar :: Id -> TCType -> (Context -> Context)
updateVar id ty = M.insert (identOf id) (Variable (locOf id) ty)

updateConst :: Id -> TCType -> (Context -> Context)
updateConst id ty = M.insert (identOf id) (Constant (locOf id) ty)

updateFun :: Id -> TCType -> [FormalParam] -> (Context -> Context)
updateFun id ty params =
   let
      p2a = \(Param pby ty id) -> (Arg (locOf id) pby (tcTypeOf ty) (identOf id))
      args = p2a <$> params
   in
      M.insert (identOf id) (Function (locOf id) args ty)

checkExpWith :: (a -> Env -> Err TCType) -> (a -> TCype -> Env -> Err ())
checkExpWith inferer = \exp typ env -> do
   typ' <- inferer exp env
   guard (typ == typ')

checkLogical :: RExp -> RExp -> (Env -> Err TCType)
checkLogical r1 r2 = \env -> do
   checkExpWith inferRExp r1 TCBool env
   checkExpWith inferRExp r2 TCBool env
   return TCBool

checkArithmetic :: RExp -> RExp -> (Env -> Err TCType)
checkArithmetic r1 r2 = \env -> do
   t1 <- inferRExp r1 env
   t2 <- inferRExp r2 env
   leastGeneral t1 t2


inferRExp :: RExp -> (Env -> Err TCType)
inferRExp exp = case exp of
   LogicalAnd r1 r2  -> checkLogical r1 r2
   LogicalOr  r1 r2  -> checkLogical r1 r2
   LogicalNot r1     -> checkExpWith inferRExp r1 TCBool
   Comparison r1 _ r2 -> \env -> do
      t1 <- inferRExp r1 env
      t2 <- inferRExp r2 env
      leastGeneral t1 t2
      return TCBool
   Sum r1 r2         -> checkArithmetic r1 r2
   Sub r1 r2         -> checkArithmetic r1 r2
   Mul r1 r2         -> checkArithmetic r1 r2
   Div r1 r2         -> checkArithmetic r1 r2
   Pow r1 r2         -> checkArithmetic r1 r2
   Mod r1 r2         -> \env -> do
      t <- join $ leastGeneral <$> checkArithmetic r1 r2 env <*> Ok TCInt
      guard (t == TCInt)
   Sign _ r          -> \env -> join $ leastGeneral TCInt <$> inferRExp r env
--   Reference lexp          -> inferLExp lexp env  ?? CHE MINCHIA DI TIPO HA UN PUNTATORE ??
   LRExp l           -> inferLExp l
--   CallExp id rexps        -> lookFun id rexps
   ReadExp rpred     -> (const . Ok) (tcTypeOf rpred)
   Lit lit           -> (const . Ok) (tcTypeOf lit)
   
  
inferLExp :: LExp -> (Env -> Err TCType)
inferLExp exp = case exp of
--   Dereference lexp        ->  ??? BoH???
   Post l _          -> \env -> join $ leastGeneral TCInt <$> inferLExp l env
   Pre _ l           -> \env -> join $ leastGeneral TCInt <$> inferLExp l env
   ArrayAccess l r   -> \env -> do
      join $ leastGeneral TCInt <$> inferRExp r env
      inferLExp l env
--   IdExp id                -> lookVar id env    ?? o lookConst ??

leastGeneral :: TCType -> TCType -> Err TCType
leastGeneral x y =
   case (x <=. y) of
      Ok True -> Ok y
      Ok False -> Ok x
      _ -> Bad $ "Error: " ++ (show x) ++ " and " ++ (show y) ++ " are not compatible."