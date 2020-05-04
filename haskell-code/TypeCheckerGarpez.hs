module TypeChecker where

import qualified Data.Map.Strict as M
import AbsGarpez
import ErrM


-- // DATA TYPES //////////////////////////////////////////////////////////
type Ident = String
type Env = [Context]
type Context = M.Map Ident EnvEntry
data EnvEntry
   = Variable Loc Type
   | Function Loc [Arg] RetType
   | Constant Loc Type
   deriving (Show)

data Arg = Arg Loc PassBy Type Ident
   deriving (Show)

data Loc = Loc {line, column :: Int}
   deriving (Show)



-- CLASS DEFINITIONS //////////////////////////////////////////////////////
class Identifiable a where
   identOf :: a -> Ident

class Localizable a where
   locOf :: a -> Loc


-- INSTANCES //////////////////////////////////////////////////////////////
instance Identifiable Id where
   identOf (Id (_, id)) = id

instance Localizable Id where
   locOf (Id ((l, c), _)) = Loc l c

instance Localizable EnvEntry where
   locOf (Variable loc _) = loc
   locOf (Function loc _ _) = loc
   locOf (Constant loc _) = loc


-- ////////////////////////////////////////////////////////////////////////

updateVar :: Id -> Type -> (Context -> Context)
updateVar id ty = M.insert (identOf id) (Variable (locOf id) ty)

updateConst :: Id -> Type -> (Context -> Context)
updateConst id ty = M.insert (identOf id) (Constant (locOf id) ty)

updateFun :: Id -> RetType -> [FormalParam] -> (Context -> Context)
updateFun id ty params =
   let
      p2a = (\(Param pby ty id) -> (Arg (locOf id) pby ty (identOf id)))
      args = map p2a params
   in
      M.insert (identOf id) (Function (locOf id) args ty)

checkRExp :: RExp -> Type -> Env -> Err ()
checkRExp exp typ env = do
           typ' <- inferRExp exp env
           if (typ' == typ) then
              return ()
           else
              Bad "type error"

checkLExp :: LExp -> Type -> Env -> Err ()
checkLExp exp typ env = do
           typ' <- inferLExp exp env
           if (typ' == typ) then
              return ()
           else
              Bad "type error"


inferRExp :: RExp -> Env -> Err Type
inferRExp exp env = case exp of
   LogicalAnd rexp1 rexp2  -> do
                               checkRExp rexp1 (SType SimpleType_bool) env
                               checkRExp rexp2 (SType SimpleType_bool) env
                               return (SType SimpleType_bool)
   LogicalOr rexp1 rexp2   -> do
                               checkRExp rexp1 (SType SimpleType_bool) env
                               checkRExp rexp2 (SType SimpleType_bool) env
                               return (SType SimpleType_bool)
   LogicalNot rexp         -> do
                               checkRExp rexp (SType SimpleType_bool) env
                               return (SType SimpleType_bool)
   Comparison rexp1 compop rexp2 -> do
                               t1 <- inferRExp rexp1 env
                               t2 <- inferRExp rexp2 env
                               leastGeneral t1 t2
                               return (SType SimpleType_bool)
   Sum rexp1 rexp2         -> do
                               t1 <- inferRExp rexp1 env
                               t2 <- inferRExp rexp2 env
                               leastGeneral t1 t2
   Sub rexp1 rexp2         -> do
                               t1 <- inferRExp rexp1 env
                               t2 <- inferRExp rexp2 env
                               leastGeneral t1 t2
   Mul rexp1 rexp2         -> do
                               t1 <- inferRExp rexp1 env
                               t2 <- inferRExp rexp2 env
                               leastGeneral t1 t2
   Div rexp1 rexp2         -> do
                               t1 <- inferRExp rexp1 env
                               t2 <- inferRExp rexp2 env
                               leastGeneral t1 t2
   Pow rexp1 rexp2         -> do
                               t1 <- inferRExp rexp1 env
                               t2 <- inferRExp rexp2 env
                               leastGeneral t1 t2
   Mod rexp1 rexp2         -> do
                               t1 <- inferRExp rexp1 env
                               t2 <- inferRExp rexp2 env
                               t <- leastGeneral t1 t2
                               t' <- leastGeneral t (SType SimpleType_int)
                               if (t' == (SType SimpleType_int)) then
                                  return (SType SimpleType_int)
                               else
                                  Bad "warning"
   Sign signop rexp        -> do
                               t <- inferRExp rexp env
                               leastGeneral t (SType SimpleType_int)
--   Reference lexp          -> inferLExp lexp env  ?? CHE MINCHIA DI TIPO HA UN PUNTATORE ??
   LRExp lexp              -> inferLExp lexp env
--   CallExp id rexps        -> lookFun id rexps
   ReadExp rpred           -> case rpred of 
                                (RPredefinedRChar _) -> Ok (SType SimpleType_char)
                                (RPredefinedRInt _) -> Ok (SType SimpleType_int)
                                (RPredefinedRFloat _) -> Ok (SType SimpleType_float)
                                (RPredefinedRString _) -> Ok (SType SimpleType_string)
   Lit literal             -> case literal of
                                (LiteralPBool _) -> Ok (SType SimpleType_bool)
                                (LiteralPChar _) -> Ok (SType SimpleType_char)
                                (LiteralPInt _) -> Ok (SType SimpleType_int)
                                (LiteralPFloat _) -> Ok (SType SimpleType_float)
                                (LiteralPString _) -> Ok (SType SimpleType_string)
   
  
inferLExp :: LExp -> Env -> Err Type
inferLExp exp env = case exp of
--   Dereference lexp        ->  ??? BoH???
   Post lexp incdecop      -> do
                               t <- inferLExp lexp env
                               t' <- leastGeneral t (SType SimpleType_int)
                               if (t' == (SType SimpleType_int)) then
                                  return t
                               else
                                  Bad "warning"
   Pre incdecop lexp       -> do
                               t <- inferLExp lexp env
                               t' <- leastGeneral t (SType SimpleType_int)
                               if (t' == (SType SimpleType_int)) then
                                  return t
                               else
                                  Bad "warning"
   ArrayAccess lexp rexp   -> do
                               trexp <- inferRExp rexp env
                               t <- leastGeneral trexp (SType SimpleType_int)
                               if (t == (SType SimpleType_int)) then
                                  inferLExp lexp env
                               else
                                  Bad "warning"
--   IdExp id                -> lookVar id env    ?? o lookConst ??


leastGeneral :: Type -> Type -> Err Type
leastGeneral (SType SimpleType_char) (SType SimpleType_char) = Ok (SType SimpleType_char)
leastGeneral (SType SimpleType_char) (SType SimpleType_int) = Ok (SType SimpleType_int)
leastGeneral (SType SimpleType_char) (SType SimpleType_float) = Ok (SType SimpleType_float)
leastGeneral (SType SimpleType_int) (SType SimpleType_char) = Ok (SType SimpleType_int)
leastGeneral (SType SimpleType_int) (SType SimpleType_int) = Ok (SType SimpleType_int)
leastGeneral (SType SimpleType_int) (SType SimpleType_float) = Ok (SType SimpleType_float)
leastGeneral (SType SimpleType_float) (SType SimpleType_char) = Ok (SType SimpleType_float)
leastGeneral (SType SimpleType_float) (SType SimpleType_int) = Ok (SType SimpleType_float)
leastGeneral (SType SimpleType_float) (SType SimpleType_float) = Ok (SType SimpleType_float)
leastGeneral (SType SimpleType_bool) (SType SimpleType_bool) = Ok (SType SimpleType_bool)
leastGeneral (SType SimpleType_string) (SType SimpleType_string) = Ok (SType SimpleType_string)
leastGeneral _ _ = Bad "warning: types not compatible"