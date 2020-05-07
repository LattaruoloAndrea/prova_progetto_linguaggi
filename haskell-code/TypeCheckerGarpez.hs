module TypeChecker where

import qualified Data.Map.Strict as M
import AbsGarpez
import ErrM
import Control.Monad (guard, liftM2, join, foldM)
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

instance Identifiable InitItem where
   identOf (InitDecl id _) = identOf id

instance Localizable Id where
   locOf (Id ((l, c), _)) = Loc l c

instance Localizable EnvEntry where
   locOf (Variable loc _) = loc
   locOf (Function loc _ _) = loc
   locOf (Constant loc _) = loc

instance Localizable InitItem where
   locOf (InitDecl id _) = locOf id

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

-- updateVar :: Id -> Type -> (Context -> Context)
-- updateVar id ty = M.insert (identOf id) (Variable (locOf id) ty)

-- updateConst :: Id -> Type -> (Context -> Context)
-- updateConst id ty = M.insert (identOf id) (Constant (locOf id) ty)


-- updateVar/updateConst generalizzate: f qui può essere una fra "Variable" e "Constant"
-- Da pensarci per quanto riguarda updateFun
updateWith :: (Loc -> Type -> EnvEntry) -> Id -> Type -> (Context -> Context)
updateWith f id ty = M.insert (identOf id) (f (locOf id) ty)

updateFun :: Id -> Type -> [FormalParam] -> (Context -> Context)
updateFun id ty params =
   let
      p2a = \(Param pby ty id) -> (Arg (locOf id) pby ty (identOf id))
      args = p2a <$> params
   in
      M.insert (identOf id) (Function (locOf id) args ty)

-- check di un'espressione (LExp o RExp che sia) tramite una funzione "inferitrice"
checkExpWith :: (a -> (Env -> Err Type)) -> (a -> Type -> Env -> Err ())
checkExpWith inferer = \exp typ env -> do
   typ' <- inferer exp env
   guard (typ == typ')

-- inferisce espressione binaria booleane (&& ||)
inferLogical :: RExp -> RExp -> (Env -> Err Type)
inferLogical r1 r2 = \env -> do
   checkExpWith inferRExp r1 bool env
   checkExpWith inferRExp r2 bool env
   return bool


-- inferisce espressione binaria aritmetiche (+ - * / ^)
inferArithmetic :: RExp -> RExp -> (Env -> Err Type)
inferArithmetic r1 r2 = \env -> do
   t1 <- inferRExp r1 env
   t2 <- inferRExp r2 env
   leastGeneral t1 t2


-- inferisce RExp
inferRExp :: RExp -> (Env -> Err Type)
inferRExp exp = case exp of
   LogicalAnd r1 r2  -> inferLogical r1 r2
   LogicalOr  r1 r2  -> inferLogical r1 r2
   LogicalNot r1     -> \env -> (checkExpWith inferRExp r1 bool env) >> (Ok bool)
   Comparison r1 _ r2 -> \env -> do
      t1 <- inferRExp r1 env
      t2 <- inferRExp r2 env
      leastGeneral t1 t2
      return bool
   Sum r1 r2         -> inferArithmetic r1 r2
   Sub r1 r2         -> inferArithmetic r1 r2
   Mul r1 r2         -> inferArithmetic r1 r2
   Div r1 r2         -> inferArithmetic r1 r2
   Pow r1 r2         -> inferArithmetic r1 r2
   Mod r1 r2         -> \env -> do -- Per il mod facciamo oltre ad inferire imponiamo che il tipo generale sia int
      t <- join $ leastGeneral <$> inferArithmetic r1 r2 env <*> Ok int
      guard (t == int)
      Ok int
   Sign _ r          -> \env -> join $ leastGeneral int <$> inferRExp r env
--   Reference lexp          -> inferLExp lexp env  ?? CHE MINCHIA DI TIPO HA UN PUNTATORE ??
   LRExp l           -> inferLExp l
--   CallExp id rexps        -> lookFun id rexps
   ReadExp rpred     -> (const . Ok) (typeOf rpred)
   Lit lit           -> (const . Ok) (typeOf lit)
   
  
-- inferisce LExp
inferLExp :: LExp -> (Env -> Err Type)
inferLExp exp = case exp of
--   Dereference lexp        ->  ??? BoH???
   Post l _          -> \env -> join $ leastGeneral int <$> inferLExp l env
   Pre _ l           -> \env -> join $ leastGeneral int <$> inferLExp l env
   ArrayAccess l r   -> \env -> do
      join $ leastGeneral int <$> inferRExp r env
      inferLExp l env
--   IdExp id                -> lookVar id env    ?? o lookConst ??


-- Dati due tipi, restituisce il meno generale tra i due (Ok)
-- Se i tipi non sono compatibili errore (Bad)
leastGeneral :: Type -> Type -> Err Type
leastGeneral x y =
   case (x <=. y) of
      Ok True -> Ok y
      Ok False -> Ok x
      _ -> Bad $ "Error: " ++ (show x) ++ " and " ++ (show y) ++ " are not compatible."



-- ////////////////////////////////////////////////////////////////////////

-- check di una dichiarazione: la notazione "strana" è più o meno l'equivalente di
-- (checkConstDecl consts env) : cs
-- la necessità di <$> è per permettere di concatenare in modo pulito la testa
-- di un Env monadico (Err ritornato dal check) con la tail non monadica (cs) 
-- Analogo per checkVarDecl
checkDeclaration :: Declaration -> Env -> Err Env
checkDeclaration decl env@(c:cs) = case decl of
   ConstDecl consts -> (:cs) <$> checkConstDecl consts env
   VarDecl typ vars -> (:cs) <$> checkVarDecl typ vars env


-- Dati un costruttore di EnvEntry (f che può essere Variable o Constant)
-- un Id, una RExp e un Env, aggiunge Id=RExp nel contesto (ritornandolo)
-- se era già presente Bad
initWith :: (Loc -> Type -> EnvEntry) -> Id -> RExp -> Env -> Err Context
initWith f id r (c:cs) = do
      guard . isNothing $ M.lookup (identOf id) c
      ty <- inferRExp r (c:cs)
      return $ updateWith f id ty c


-- Data una lista di InitItem xs, faccio un "foldl" su questa lista
-- accumulando il nuovo Context (e inserendo delle Constant)
-- la version monadica (foldM) è necessaria per ritornare
-- in modo pulito un Err Context
checkConstDecl :: [InitItem] -> Env -> Err Context
checkConstDecl xs (ctx:cs) = foldM f ctx xs
   where
      f = \c (InitDecl id r) -> initWith Constant id r (c:cs)


-- Dato un tipo e una lista di DeclItem xs, faccio un "foldl" su questa lista
-- accumulando il nuovo Context (e inserendo delle Variable)
-- la version monadica (foldM) è necessaria per ritornare
-- in modo pulito un Err Context
checkVarDecl :: Type -> [DeclItem] -> Env -> Err Context
checkVarDecl ty xs (ctx:cs) = foldM f ctx xs
   where
      f = \c dIt -> case dIt of
         (DeclItemDeclId (DeclOnly id)) -> do
            guard . isNothing $ M.lookup (identOf id) c
            return $ updateWith Variable id ty c
         (DeclItemInitItem (InitDecl id r)) -> do
            t <- inferRExp r [c]
            case ty >=. t of
               (Ok True) -> initWith Variable id r (c:cs)
               _         -> Bad "Not compatible."



-- checkVar :: Type -> Id -> Env -> Err Env
-- checkVar typ id env = let
--       val = M.lookup (identOf id) (head env) in
--       case val of
--          Nothing -> let
--                      x' = updateVar id typ (head env) in
--                      return (x' : (tail env))
--          (Just _) -> let x' = updateVar id typ (head env)
--                      in do
--                        Bad "warning: identifier already used"
--                        return (x' : (tail env))

-- checkInitVar :: Type -> Id -> RExp -> Env -> Err Env
-- checkInitVar typ id rexp env = let
--       val = M.lookup (identOf id) (head env) in
--       case val of
--           Nothing -> let
--                        typ' = inferRExp rexp env in
--                        if (typ == typ') then let
--                           x' = updateVar id typ (head env)
--                           in return (x' : (tail env))
--                        else
--                           Bad ("variable" ++ (show id) ++ "has type different from declared")   -- int a = "ciao"
--           (Just _) -> do
--                        Bad "warning: identifier already used"
--                        typ' <- inferRExp rexp env
--                        if (typ == typ') then let
--                           x' = updateVar id typ (head env)
--                           in return (x' : (tail env))
--                        else
--                           Bad ("variable" ++ (identOf id) ++ "has type different from declared")   -- int a = "ciao"


-- lookVar :: Id -> Env -> Err Type
-- lookVar id env = let
--       kind = head' (lookupList (identOf id) env) in
--       case kind of
--          Nothing                            -> Bad ((identOf id) ++ "never declared")
--          (Just (Variable loc typ))          -> Ok typ
--          (Just (Function loc params typ))   -> Bad ((identOf id) ++ "is a function")
--          (Just (Constant loc typ))          -> Bad ((identOf id) ++ "is a constant")


lookupList :: Ident -> (Env -> Maybe EnvEntry)
lookupList k = head' . dropWhile isNothing . map (M.lookup k)

head' :: [Maybe a] -> Maybe a
head' [] = Nothing
head' xs = head xs