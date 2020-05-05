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
   = Variable Loc Type
   | Function Loc [Arg] Type
   | Constant Loc Type
   deriving (Show)

data Arg = Arg Loc PassBy Type Ident
   deriving (Show)

data Loc = Loc {line, column :: Int}
   deriving (Show)


-- TYPE ALIASES //////////////////////////////////////////////////////////

bool = typeOf SimpleType_bool
char = typeOf SimpleType_char
int  = typeOf SimpleType_int
float = typeOf SimpleType_float
string = typeOf SimpleType_string
void = typeOf SimpleType_void


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


class Typeable a where
   typeOf :: a -> Type

-- INSTANCES //////////////////////////////////////////////////////////////
instance Identifiable Id where
   identOf (Id (_, id)) = id

instance Localizable Id where
   locOf (Id ((l, c), _)) = Loc l c

instance Localizable EnvEntry where
   locOf (Variable loc _) = loc
   locOf (Function loc _ _) = loc
   locOf (Constant loc _) = loc

instance PartialOrd SimpleType where
   x <=. y
      | x == y = Ok True
      | typeOf x `elem` [bool, string, void] = Bad "Not compatible."
      | typeOf y `elem` [bool, string, void] = Bad "Not compatible."
      | otherwise = Ok (x <= y)

instance PartialOrd Type where
   (SType x) <=. (SType y) = x <=. y
   (AType t1 (PInt d1)) <=. (AType t2 (PInt d2))
      | (snd d1) == (snd d2) = t1 <=. t2
      | otherwise = Bad "Not compatible"
   _ <=. _ = Bad "Not compatible"


instance Typeable RPredefined where
   typeOf x = case x of
      (RPredefinedRChar _) -> (SType SimpleType_char)
      (RPredefinedRInt _)  -> (SType SimpleType_int)
      (RPredefinedRFloat _) -> (SType SimpleType_float)
      (RPredefinedRString _) -> (SType SimpleType_string)

instance Typeable Literal where
   typeOf x = case x of
      (LiteralPBool _) -> (SType SimpleType_bool)
      (LiteralPChar _) -> (SType SimpleType_char)
      (LiteralPInt _) -> (SType SimpleType_int)
      (LiteralPFloat _) -> (SType SimpleType_float)
      (LiteralPString _) -> (SType SimpleType_string)


instance Typeable SimpleType where
   typeOf = SType



-- ////////////////////////////////////////////////////////////////////////

updateVar :: Id -> Type -> (Context -> Context)
updateVar id ty = M.insert (identOf id) (Variable (locOf id) ty)

updateConst :: Id -> Type -> (Context -> Context)
updateConst id ty = M.insert (identOf id) (Constant (locOf id) ty)

updateFun :: Id -> Type -> [FormalParam] -> (Context -> Context)
updateFun id ty params =
   let
      p2a = \(Param pby ty id) -> (Arg (locOf id) pby ty (identOf id))
      args = p2a <$> params
   in
      M.insert (identOf id) (Function (locOf id) args ty)

checkExpWith :: (a -> (Env -> Err Type)) -> (a -> Type -> Env -> Err ())
checkExpWith inferer = \exp typ env -> do
   typ' <- inferer exp env
   guard (typ == typ')

checkLogical :: RExp -> RExp -> (Env -> Err Type)
checkLogical r1 r2 = \env -> do
   checkExpWith inferRExp r1 bool env
   checkExpWith inferRExp r2 bool env
   return bool

checkArithmetic :: RExp -> RExp -> (Env -> Err Type)
checkArithmetic r1 r2 = \env -> do
   t1 <- inferRExp r1 env
   t2 <- inferRExp r2 env
   leastGeneral t1 t2


inferRExp :: RExp -> (Env -> Err Type)
inferRExp exp = case exp of
   LogicalAnd r1 r2  -> checkLogical r1 r2
   LogicalOr  r1 r2  -> checkLogical r1 r2
   LogicalNot r1     -> \env -> (checkExpWith inferRExp r1 bool env) >> (Ok bool)
   Comparison r1 _ r2 -> \env -> do
      t1 <- inferRExp r1 env
      t2 <- inferRExp r2 env
      leastGeneral t1 t2
      return bool
   Sum r1 r2         -> checkArithmetic r1 r2
   Sub r1 r2         -> checkArithmetic r1 r2
   Mul r1 r2         -> checkArithmetic r1 r2
   Div r1 r2         -> checkArithmetic r1 r2
   Pow r1 r2         -> checkArithmetic r1 r2
   Mod r1 r2         -> \env -> do
      t <- join $ leastGeneral <$> checkArithmetic r1 r2 env <*> Ok int
      guard (t == int)
      Ok int
   Sign _ r          -> \env -> join $ leastGeneral int <$> inferRExp r env
--   Reference lexp          -> inferLExp lexp env  ?? CHE MINCHIA DI TIPO HA UN PUNTATORE ??
   LRExp l           -> inferLExp l
--   CallExp id rexps        -> lookFun id rexps
   ReadExp rpred     -> (const . Ok) (typeOf rpred)
   Lit lit           -> (const . Ok) (typeOf lit)
   
  
inferLExp :: LExp -> (Env -> Err Type)
inferLExp exp = case exp of
--   Dereference lexp        ->  ??? BoH???
   Post l _          -> \env -> join $ leastGeneral int <$> inferLExp l env
   Pre _ l           -> \env -> join $ leastGeneral int <$> inferLExp l env
   ArrayAccess l r   -> \env -> do
      join $ leastGeneral int <$> inferRExp r env
      inferLExp l env
--   IdExp id                -> lookVar id env    ?? o lookConst ??

leastGeneral :: Type -> Type -> Err Type
leastGeneral x y =
   case (x <=. y) of
      Ok True -> Ok y
      Ok False -> Ok x
      _ -> Bad $ "Error: " ++ (show x) ++ " and " ++ (show y) ++ " are not compatible."