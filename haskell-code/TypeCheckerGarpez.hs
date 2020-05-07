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

updateWith :: (Loc -> Type -> EnvEntry) -> Id -> Type -> (Context -> Context)
updateWith f id ty = M.insert (identOf id) (f (locOf id) ty)

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



-- ////////////////////////////////////////////////////////////////////////
checkProgram :: Program -> Err Env
checkProgram (Prog globs) = checkGlobal globs [M.empty]

checkGlobal :: [Global] -> Env -> Err Env
checkGlobal [] env = Ok env
checkGlobal (x:xs) env = case x of
   GlobalDecl decl -> let 
     env' = checkDeclaration decl env
     in case env' of
        (Ok e) -> checkGlobal xs e
        (Bad s) -> Bad s
{-   FunDecl fun -> let
     env' = checkFunction fun env
     in checkGlobal xs env'
-}

checkDeclaration :: Declaration -> Env -> Err Env
checkDeclaration decl env@(c:cs) = case decl of
   ConstDecl consts -> (:cs) <$> checkConstDecl consts env
   VarDecl typ vars -> (:cs) <$> checkVarDecl typ vars env


initWith :: (Loc -> Type -> EnvEntry) -> Id -> RExp -> Env -> Err Context
initWith f id r (c:cs) = do
      guard . isNothing $ M.lookup (identOf id) c
      ty <- inferRExp r (c:cs)
      return $ updateWith f id ty c


checkConstDecl :: [InitItem] -> Env -> Err Context
checkConstDecl xs (ctx:cs) = foldM f ctx xs
   where
      f = \c (InitDecl id r) -> initWith Constant id r (c:cs)

checkVarDecl :: Type -> [DeclItem] -> Env -> Err Context
checkVarDecl ty xs (ctx:cs) = foldM f ctx xs
   where
      f = \c dIt -> case dIt of
         (DeclItemDeclId (DeclOnly id)) -> do
            guard . isNothing $ M.lookup (identOf id) c
            return $ updateWith Variable id ty c
         (DeclItemInitItem (InitDecl id r)) -> initWith Variable id r (c:cs)

{-
checkFun :: Function -> Env -> Err Env
checkFun (Fun rettyp (FRest id params blk)) env = let
         val = M.lookup (identOf id) (head env) in
         case val of
            Nothing -> let 
                        x' = updateFun id rettyp params (head env)
                        startblk = addParams params [[] ++ (x : (tail env))]
                        (env', ret) = checkBlock blk startblk False
                        in if ((typeOf rettype) == void || ret == True) then
                           return env -- dovrebbe essere uguale a env'
                        else
                           Bad "error"
            (Just _) -> do
                        Bad "warning"
                        let 
                        x' = updateFun id rettyp params (head env)
                        startblk = addParams params [[] ++ (x : (tail env))]
                        (env', ret) = checkBlock blk startblk False
                        in if ((typeOf rettype) == void || ret == True) then
                           return env -- dovrebbe essere uguale a env'
                        else
                           Bad "error"

addParams :: [FormalParam] -> Env -> Err Env
addParams [] env = Ok env
addParams ((Param pass typ id) : xs) = case pass of
   ValuePass -> ...
   RefPass -> ...


checkBlock :: Block -> Env -> Bool -> Type -> (Err Env, Bool)
checkBlock (Blk [] []) env ret rettyp = return (tail(env), ret)
checkBlock (Blk (x:xs) ys) env ret rettyp = let
         env' = checkDeclaration x env
         in case env' of
             (Ok env'')  -> checkBlock (Blk xs ys) env'' False rettyp
             (Bad s) -> do
                         Bad s
                         checkBlock (Blk xs ys) env'' False rettyp
checkBlock (Blk [] (y:ys)) env ret rettyp = 
         if (ret == True) then 
            return (tail(env), ret)
         else
            let (env', ret') = checkStm y env False rettyp
            in
            case env' of
               (Ok env'') -> return (checkBlock (Blk [] ys) env' (ret || ret') rettyp)
               (Bad s) -> do
                         Bad s
                         checkBlock (Blk [] ys) env' (ret || ret') rettyp
                        
checkStm :: Statement :: Stm -> Env -> Type -> (Err Env, Bool)   -- oppure Err (Env, Bool)?
checkStm stm env rettyp = case stm of
   BlkStm blk -> checkBlock blk env False rettyp
   CallStm id rexps -> let
                       val = lookupList id env
                       in case val of
      (Just (Function loc params typ)) -> checkParams rexps params env
      (Just _) -> Bad (show id) ++ "is not a function"
      Nothing -> Bad "function" ++ (show id) ++ "not defined"
   Assign lexp assop rexp -> let
                             t1 = inferLExp lexp env
                             t2 = inferRExp rexp env
                             in case t1 of
                                (Ok t1') -> case t2 of
                                   (Ok t2') -> if (t1' == t2' ... leastGeneral) then
                                      return (env, False)
                                   else
                                      Bad " ... "
                                   (Bad s) -> (Bad s, False)
                                (Bad s1) -> (Bad s1, False)
   LExp lexp -> case lexp of
      Dereference _ -> do
                        Bad "..."
                        return (env, False)
      ArrayAccess _ -> do
                        Bad "..."
                        return (env, False)
      IdExp _       -> do
                        Bad "..."
                        return (env, False)
      Post lexp incdec -> do
                           checkExpWith lexp env int
                           return (env, False)
      Pre incdec lexp  -> do
                           checkExpWith lexp env int
                           return (env, False)
   CondStm cond -> checkConditional cond env rettyp   -- non devo passare return (?)
   LoopStm loop -> case loop of
      LoopWhile (WhileLoop rexp blk)     -> do
                                             checkExpWith rexp env (typeOf bool)
                                             let
                                             (env', ret') = checkBlockWhile blk rettyp env False
                                             in case env' of
                                                (Ok env'') -> return (env'', False) -- se un while è returning non importa: potrei non entrarci
                                                (Bad s) -> return (Bad s, False)
      LoopDoWhile (DoWhileLoop blk rexp) -> do
                                             checkExpWith rexp env (typeOf bool)
                                             let
                                             (env', ret') = checkBlockWhile blk rettyp env False -- uguale a checkblock, ma permette break e return
                                             in case env' of
                                                (Ok env'') -> return (env'', False) -- se un while è returning non importa: potrei non entrarci
                                                (Bad s) -> return (Bad s, False)
      LoopFor (ForLoop decl rexp1 rexp2 blk) -> do
                                             checkDeclaration decl env
                                             checkExpWith rexp1 env (typeOf bool)
                                             checkExpWith rexp1 env (typeOf bool)
                                             let
                                             (env', ret') = checkBlock blk rettyp env False
                                             in case env' of
                                                (Ok env'') -> return (env'', False) -- se un for è returning non importa: potrei non entrarci
                                                (Bad s) -> return (Bad s, False)
   JumpStm jump -> case jump of
      JumpReturn ret -> if (rettyp == (typeOf void)) then
                           return (env, True)
                        else
                           Bad "cannot return whithout type in a function"
      Jump1 ret rexp -> let
                        t = inferRExp rexp env
                        if (rettyp == (typeOf t)) then
                           return (env, True)
                        else
                           Bad "different types"
      JumpBreak brk -> Bad "cannot have breake outside a while loop"
      JumpContinue cnt -> Bad "cannot have continue outside a while loop"
   WriteStm wpred recp -> let
                          t = inferRExp rexp env
                          case wpred of
      (WPredefinedWChar ch) -> if (t == (typeOf char)) then
                                  return (Env, False)
                               else
                                  Bad "different types"
      (WPredefinedWInt in) -> if (t == (typeOf int)) then
                                  return (Env, False)
                               else
                                  Bad "different types"
      (WPredefinedWFloat fl) -> if (t == (typeOf float)) then
                                  return (Env, False)
                               else
                                  Bad "different types"
      (WPredefinedWString st) -> if (t == (typeOf string)) then
                                  return (Env, False)
                               else
                                  Bad "different types"

checkParams :: [RExp] -> [Arg] -> Env -> Err ()
checkParams [] [] env = Ok ()
checkParams [] ys env = Bad "different number of args"
checkParams xs [] env = Bad "different number of args"
checkParams (x:xs) ((Arg loc passby typ id):ys) env = let 
                      t = inferRExp x env
                      if (x == typ) then
                         checkParams xs ys env
                      else
                         Bad "expression" ++ (show x) ++ "has different type from arg" ++ (show id)

checkBlockWhile :: Block -> Env -> Bool -> Type -> (Err Env, Bool)
checkBlockWhile (Blk [] []) env ret rettyp = return (tail(env), ret)
checkBlockWhile (Blk (x:xs) ys) env ret rettyp = let
         env' = checkDeclaration x env
         in case env' of
             (Ok env'')  -> checkBlockWhile (Blk xs ys) env'' False rettyp
             (Bad s) -> do
                         Bad s
                         checkBlockWhile (Blk xs ys) env'' False rettyp
checkBlockWhile (Blk [] (y:ys)) env ret rettyp = 
         if (ret == True) then 
            return (tail(env), ret)
         else
            let (env', ret') = checkStmWhile y env False rettyp
            in
            case env' of
               (Ok env'') -> return (checkBlockWhile (Blk [] ys) env' (ret || ret') rettyp)
               (Bad s) -> do
                         Bad s
                         checkBlockWhile (Blk [] ys) env' (ret || ret') rettyp

checkStmWhile :: Statement :: Stm -> Env -> Type -> (Err Env, Bool)   -- oppure Err (Env, Bool)?
checkStmWhile stm env rettyp = case stm of
   JumpStm (JumpBreak brk) -> (Ok env, False)
   JumpStm (JumpContinue cnt) -> (Ok env, False)
   -- gli altri casi con i blocchi vanno definiti chiamando checkBlockWhile
   _ -> checkStm stm env rettyp

checkConditional :: Conditional -> Type -> Env -> (Err Env, Bool)
checkConditional cond rettyp env = case cond of
   ConditionalIf if -> checkIf if rettyp env
-- eventualmente altri condizionali

checkIf :: If -> Type -> Env -> (Err Env, Bool)
checkIf (IfCond rexp blk rest) rettyp env = let
                         env1 = checkRExpWith rexp env (typeOf bool)
                         (env2, ret) = checkBlock blk env False rettyp
                         in case rest of
   RestIf_     -> return (env2, ret)
   RestIf1 if  -> let
                  (env3, ret1) = checkIf if rettyp env
                  return (env3, ret && ret1)
   RestIf2 blk -> let
                  (env4, ret2) = checkBlock blk env False rettyp
                  return (env4, ret && ret2)
-}


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
