module TypeCheckerGarpez where

import qualified Data.Map as Map
import AbsGarpez

type Ident = String
type Env = [ SingleEnv ]
type SingleEnv = Map.Map Ident EnvEntry
data EnvEntry = Variable Loc Type
              | Function Loc [Parameter] RetType
              | Constant Loc Type  deriving (Show)

data Parameter = Parameter Loc PassBy Type Ident   deriving (Show)

data Loc = Loc {line, column :: Int}   deriving (Show)


data Err a = Ok a | Bad String

updateVar :: Id -> Type -> (SingleEnv -> SingleEnv)
updateVar (Id ((l, c), ident)) typ = Map.insert ident (Variable (Loc l c) typ)

updateConst :: Id -> Type -> (SingleEnv -> SingleEnv)
updateConst (Id ((l, c), ident)) typ = Map.insert ident (Constant (Loc l c) typ)

updateFun :: Id -> RetType -> [ FormalParam ] -> (SingleEnv -> SingleEnv)
updateFun (Id ((l, c), ident)) rettyp [ (Param passby typ (Id ((lp, cp), identp)) ) ] = 
                   Map.insert ident (Function (Loc l c) [ (Parameter (Loc lp cp) passby typ identp) ] rettyp)

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
