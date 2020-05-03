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

{-
checkExp :: a -> Type -> Env -> Err ()
checkExp exp typ env = do
           typ' <- infer exp env
           if (typ' == typ) then
              return ()
           else
              fail $ "type error"


infer :: a -> Env -> Err Type
infer exp env = case exp of
   LogicalOr rexp1 rexp2   -> do
                               checkExp rexp1 (SType SimpleType_bool) env
                               checkExp rexp2 (SType SimpleType_bool) env
                               return (SType SimpleType_bool)
   LogicalOr rexp1 rexp2   -> do
                               checkExp rexp1 (SType SimpleType_bool) env
                               checkExp rexp2 (SType SimpleType_bool) env
                               return (SType SimpleType_bool)

-}
