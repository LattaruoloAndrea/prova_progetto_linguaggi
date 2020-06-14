module TacGenerator where

import Tac
import Control.Monad.Trans.State
import Control.Monad (foldM, (<=<))
import Control.Applicative (liftA, liftA2)
import Locatable
import TCType
import TCInstances
import qualified AbsChapel as A
import qualified Data.DList as DL
import qualified Data.Map.Lazy as M
import Data.Maybe


type ProgramT   = A.Program TCType
type DeclT      = A.Decl TCType
type CDeclT     = A.CDecl TCType
type VDeclT     = A.VDecl TCType
type RExpT      = A.RExp TCType
type LExpT      = A.LExp TCType
type StmT       = A.Stm TCType
type ArithOpT   = A.ArithOp TCType
type CompOpT    = A.CompOp TCType
type AssignOpT  = A.AssignOp TCType
type BlockT     = A.Block TCType
type Ident      = A.Ident
type Intent     = A.Intent
type JumpT      = A.Jump TCType
type FormT      = A.Form TCType


type Stream = DL.DList TAC          -- The list of TAC instructions
type Id     = String
type EnvT   = M.Map Id Intent
type EnvF   = M.Map Id [(Ident, TCType)]

-- The state is (counter_tempaddr, counter_label, label_break, label_continue, id_fun, list_static_data, table_intents, table_res)
type SGen a = State (Int, Int, Label, Label, Id, [Static], EnvT, EnvF) a

fall :: Label
fall = Label ""

nilCont :: LAddr -> LAddr -> Over -> Stream -> Stream
nilCont x y ov = mappend $ DL.fromList [Nil x y ov]

refCont :: LAddr -> LAddr -> Stream -> Stream
refCont x y = mappend $ DL.fromList [Ref x y]

binCont :: BinOp -> Over -> LAddr -> LAddr -> LAddr -> Stream -> Stream
binCont bop ov x y z = mappend $ DL.fromList [Bin x y bop z ov]

unCont :: UnOp -> Over -> LAddr -> LAddr -> Stream -> Stream
unCont uop ov x y = mappend $ DL.fromList [Un x uop y ov]

orCont :: LAddr -> LAddr -> LAddr -> Stream -> Stream
orCont = binCont Or B

andCont :: LAddr -> LAddr -> LAddr -> Stream -> Stream
andCont = binCont And B

notCont :: LAddr -> LAddr -> Stream -> Stream
notCont = unCont Not B

negCont :: Over -> LAddr -> LAddr -> Stream -> Stream
negCont ov = unCont (Neg ov) ov

coerceCont :: Over -> Over -> LAddr -> LAddr -> Stream -> Stream
coerceCont from to = unCont (Coerce from to) to

gotoCont :: Label -> Stream -> Stream
gotoCont lab = mappend $ DL.fromList [Goto lab]

callCont :: LAddr -> Int -> Stream -> Stream
callCont a n = mappend $ DL.fromList [Call a n]

fcallCont :: LAddr -> LAddr -> Int -> Stream -> Stream
fcallCont x f n = mappend $ DL.fromList [FCall x f n]

paramCont :: LAddr -> Stream -> Stream
paramCont x = mappend $ DL.fromList [Par x]

arithCont :: ArithOpT -> LAddr -> LAddr -> LAddr -> Stream -> Stream
arithCont op = binCont caseOp ov
    where ov = overFromTC $ tctypeOf op
          caseOp =  case op of
            A.Add _ -> Add ov
            A.Sub _ -> Sub ov
            A.Mul _ -> Mul ov
            A.Div _ -> Div ov
            A.Mod   -> Mod
            A.Pow _ -> Pow ov

compCont :: CompOpT -> LAddr -> LAddr -> LAddr -> Stream -> Stream
compCont op = binCont (Rel $ toCompOp op) B

labCont :: Label -> Stream -> Stream
labCont lab = mappend $ DL.fromList [Lab lab]



ifCont :: LAddr -> Label -> Stream -> Stream
ifCont addr lab = mappend $ DL.fromList [If addr lab]

ifFalseCont :: LAddr -> Label -> Stream -> Stream
ifFalseCont addr lab = mappend $ DL.fromList [IfFalse addr lab]

ifRelCont :: LAddr -> CompOp -> LAddr -> Label -> Stream -> Stream
ifRelCont a1 op a2 lab = mappend $ DL.fromList [IfRel a1 op a2 lab]



returnCont :: Stream -> Stream
returnCont = mappend $ DL.fromList [Return]

returnECont :: LAddr -> Stream -> Stream
returnECont x = mappend $ DL.fromList [ReturnE x]

exitCont :: Stream -> Stream
exitCont = mappend $ DL.fromList [Exit]

commentCont :: String -> Stream -> Stream
commentCont s = mappend $ DL.fromList [Comment s]


toCompOp :: CompOpT -> CompOp
toCompOp op = case op of
    A.Lt  _ -> Lt  ov
    A.Leq _ -> Leq ov
    A.Eq  _ -> Eq  ov
    A.Neq _ -> Neq ov
    A.Geq _ -> Geq ov
    A.Gt  _ -> Gt  ov
    where ov = overFromTC $ tctypeOf op

opposite :: CompOp -> CompOp
opposite op = case op of
    Lt  o -> Geq o
    Leq o -> Gt  o
    Eq  o -> Neq o
    Neq o -> Eq  o
    Geq o -> Lt  o
    Gt  o -> Leq o




-- Size of a TCType (in byte)
sizeof :: TCType -> Int
sizeof t = case t of
    TBool       -> 1
    TChar       -> 1
    TInt        -> 4
    TReal       -> 8
    TString     -> 8
    TPoint _    -> 8
    TArr d t'   -> d * (sizeof t')

-- compose a list of transformation to obtain the "concatanation" transformation
streamCat :: [SGen (Stream -> Stream)] -> SGen (Stream -> Stream)
streamCat = foldr (liftA2 (.)) (return id)

overFromTC :: TCType -> Over
overFromTC t = case t of
    TBool   -> B
    TChar   -> C
    TInt    -> I
    TReal   -> R
    _       -> P

newTemp :: SGen LAddr
newTemp = do
    (v, l, b, c, i, s, e, f) <- get
    put (v+1, l, b, c, i, s, e, f)
    return . A . ATemp . ("t"++) $ show v

newRef :: LAddr -> LAddr
newRef = RefTo . getAddr



addrFromId :: A.Ident -> LAddr
addrFromId (A.Ident loc name) = A $ AName name loc


addrFromInteger :: Integer -> LAddr
addrFromInteger = A . ALit . A.LInt


labelFromId :: A.Ident -> Label
labelFromId (A.Ident loc name) = Label $ name ++ "@" ++ (show $ A.line loc) ++ "," ++ (show $ A.column loc)


newLabel :: String -> SGen Label
newLabel s = do
    (v, l, b, c, i, ss, e, f) <- get
    put (v, l+1, b, c, i, ss, e, f)
    return . Label $ s ++ (show l)

setBreak :: Label -> SGen ()
setBreak lab = do
    (v, l, b, c, i, s, e, f) <- get
    put (v, l, lab, c, i, s, e, f)

getBreak :: SGen Label
getBreak = do
    (_, _, b, _, _, _, _, _) <- get
    return b

setContinue :: Label -> SGen ()
setContinue lab = do
    (v, l, b, c, i, s, e, f) <- get
    put (v, l, b, lab, i, s, e, f)


getContinue :: SGen Label
getContinue = do
    (_, _, _, c, _, _, _, _) <- get
    return c

setFunction :: Id -> SGen ()
setFunction fun = do
    (v, l, b, c, i, s, e, f) <- get
    put (v, l, b, c, fun, s, e, f)

getFunction :: SGen Id
getFunction = do
    (_, _, _, _, f, _, _, _) <- get
    return f

addSStr :: String -> SGen LAddr
addSStr s = do
    (v, l, b, c, i, ss, e, f) <- get
    let addr = (A . ATemp $ "ptr$str" ++ (show . length $ ss))
        sstr = SStr addr s
    put (v, l, b, c, i, sstr : ss, e, f)
    return addr

getEnvT :: SGen EnvT
getEnvT = do
    (_, _, _, _, _, _, e, _) <- get
    return e

pushEnvT :: EnvT -> SGen EnvT
pushEnvT env = do
    (a, b, c, d, i, e, old, f) <- get
    put (a, b, c, d, i, e, env, f)
    return old

pushScopeT :: [FormT] -> SGen EnvT
pushScopeT fs =
    let
        scope env = foldr (\(k, v) e -> M.insert k v e) env $ zip [A.idName id | (A.Form _ id _) <- fs] [it | (A.Form it _ _) <- fs]
    in do
        env <- getEnvT
        pushEnvT $ scope env

lookNameT :: Id -> SGen (Maybe Intent)
lookNameT s = do
    env <- getEnvT
    return $ M.lookup s env

intentOf :: Id -> SGen Intent
intentOf name = do
    intent <- lookNameT name
    case intent of
        Just x  -> return x
        Nothing -> return A.In


getEnvF :: SGen EnvF
getEnvF = do
    (_, _, _, _, _, _, _, f) <- get
    return f

pushEnvF :: EnvF -> SGen EnvF
pushEnvF env = do
    (a, b, c, d, i, e, f, old) <- get
    put (a, b, c, d, i, e, f, env)
    return old

pushFun :: DeclT -> SGen EnvF
pushFun (A.FDecl (A.Ident loc name) forms _ _ _) = do
    old <- getEnvF
    let resF = filter (\(A.Form it _ _) -> it == A.Out || it == A.InOut) forms
        idF  = map (\(A.Form _ ident ty) -> (ident, tctypeOf ty)) resF
        new  = M.insert name idF old
    pushEnvF new

lookNameF :: Id -> SGen (Maybe [(Ident, TCType)])
lookNameF id = do
    env <- getEnvF
    return $ M.lookup id env


getStatic :: SGen [Static]
getStatic = do
    (_, _, _, _, _, ss, _, _) <- get
    return ss

attachStart :: Label -> (Stream -> Stream) -> (Stream -> Stream)
attachStart lab cont = instr . cont
    where instr = mappend $ DL.fromList [Lab lab]

attachEnd :: Label -> (Stream -> Stream) -> (Stream -> Stream)
attachEnd lab cont = cont . instr
    where instr = mappend $ DL.fromList [Lab lab]

attachGuard :: Label -> (Stream -> Stream) -> (Stream -> Stream)
attachGuard lab cont = cont . gotoCont lab

addrFromArr :: LAddr -> SGen (Stream -> Stream, LAddr)
addrFromArr addr = let t = TArr 0 TVoid in case addr of
    Arr b o -> do
        tmp <- newTemp
        return (arithCont (A.Add t) tmp (A b) (A o), tmp)
    _ -> return (id, addr)



copyTo :: (LAddr, TCType) -> LAddr -> SGen (Stream -> Stream)
(source, t) `copyTo` dest = case t of
    TArr n t' -> do
        let ls = linearize t
            t' = head ls
            sz = toInteger $ sizeof t'
            linearize (TArr n t) = concatMap linearize $ take n $ repeat t
            linearize t          = [t]
        (extra, baseS) <- case source of
            Arr b o -> do
                tmp <- newTemp
                return (arithCont (A.Add t) tmp (A b) (A o), getAddr tmp)
            _       -> return (id, getAddr source)
        let copyElem = \n ->
                let off     = getAddr $ addrFromInteger $ n * sz
                    baseD   = getAddr dest
                    s       = Arr baseS off
                    d       = Arr baseD off
                in  nilCont d s $ overFromTC t'
        return $ (extra .) $ foldr (.) id $ map copyElem $ [0 .. (toInteger $ (length ls - 1))]
    
    _         -> return $ nilCont dest source $ overFromTC t



genTAC :: ProgramT -> [TAC]
genTAC program = DL.toList $ fst $ runState (genProgram program) (0, 0, fall, fall, "", [], mempty, mempty)


genProgram :: ProgramT -> SGen Stream
genProgram (A.Prog decls) = do
    let contMs = map genDecl decls
        addrM  = addrFromId $ idMain
    cont <- streamCat contMs
    sdata <- getStatic
    let sdata' = DL.fromList . reverse . map Stat $ sdata 
    return $ callCont addrM 0 . exitCont $ cont mempty `mappend` commentCont "Static Data\n" sdata'

    where
        idMain = head $ map fId $ filter isF decls
        fId (A.FDecl id _ _ _ _) = id
        isF (A.FDecl _ _ _ _ _) = True
        isF _ = False


genDecl :: DeclT -> SGen (Stream -> Stream)
genDecl decl = case decl of
    A.FDecl _ _ _ _ _ -> genFDecl decl

    A.VList vs ->
        let contMs = map genVDecl $ reverse vs
        in  streamCat contMs

    A.CList cs ->
        let contMs = map genCDecl $ reverse cs
        in  streamCat contMs



genFDecl :: DeclT -> SGen (Stream -> Stream)
genFDecl f@(A.FDecl ident forms it rt body) = do
    localInit <- streamCat $ map initializer forms
    let labStart = labCont $ labelFromId ident
        preamble = labStart . localInit
    labEnd <- newLabel "endFun"
    oldT <- pushScopeT forms
    oldF <- pushFun f
    oldI <- getFunction
    setFunction $ A.idName ident
    contBody <- genBlock body
    pushEnvT oldT
    pushEnvF oldF
    setFunction oldI
    ret <- genJump $ A.Return (A.Loc 0 0)
    
    let extraReturn = if tctypeOf rt == TVoid then ret else id
        postamble = labCont labEnd . extraReturn
    return $ \stream -> mappend stream $ preamble . contBody . postamble $ mempty
    
    where
        initializer (A.Form it ident@(A.Ident loc name) ty) =
            case it of
                A.In        -> caseIn
                A.Out       -> caseOut
                A.InOut     -> caseOut
                A.Ref       -> caseRef
                A.ConstIn   -> caseIn
                A.ConstRef  -> caseRef
            where
                caseIn  = return id
                caseOut = do
                    let addr = A $ AName (name ++ "$local$copy") loc
                    contCopy <- (addrFromId ident, tctypeOf ty) `copyTo` addr
                    return contCopy
                caseRef = return id

        


genCDecl :: CDeclT -> SGen (Stream -> Stream)
genCDecl (A.CDecl id t r) = do
    (contR, addrR) <- genRExp r
    let addrC    = addrFromId id
        contTac  = nilCont addrC addrR $ overFromTC $ tctypeOf t
        contDecl = contR . contTac
    return contDecl

genVDecl :: VDeclT -> SGen (Stream -> Stream)
genVDecl (A.Init id t r) = do
    (contR, addrR) <- genRExp r
    let addrV    = addrFromId id
        contTac  = nilCont addrV addrR $ overFromTC $ tctypeOf t
        contDecl = contR . contTac
    return contDecl


genRExp :: RExpT -> SGen (Stream -> Stream, LAddr)
genRExp r = case r of
    A.Or _ _ _ _      -> genOr r
    A.And _ _ _ _     -> genAnd r
    A.Not _ _ _       -> genNot r
    A.Comp _ _ _ _ _  -> genComp r
    A.Arith _ _ _ _ _ -> genArith r
    A.Sign _ _ _ _    -> genSign r
    A.RefE _ _ _      -> genRefE r
    A.RLExp _ _ _     -> genRLExp r
    A.ArrList _ _ _   -> genArrList r
    A.FCall _ _ _ _ _ -> genFCall r
    A.Lit _ _ _       -> genLit r
    A.Coerce _ _      -> genCoerce r


genLExp :: LExpT -> SGen (Stream -> Stream, LAddr)
genLExp l = case l of
    A.Deref  _ _   -> genDeref l
    A.Access _ _ _ -> genAccess l
    A.Name   _ _   -> genName l


genStm :: StmT -> SGen (Stream -> Stream)
genStm s = case s of
    A.StmBlock _      -> genStmBlock s
    A.StmCall  _ _ _  -> genStmCall s
    A.Assign   _ _ _  -> genAssign s
    A.StmL     _      -> return id
    A.If       _ _    -> genIf s
    A.IfElse   _ _ _  -> genIfElse s
    A.While    _ _    -> genWhile s
    A.DoWhile  _ _    -> genDoWhile s
    A.For      _ _ _  -> genFor s
    A.JmpStm   _      -> genJmpStm s







genOr :: RExpT -> SGen (Stream -> Stream, LAddr)
genOr (A.Or loc r1 r2 t) = do
    (cont1, addr1) <- genRExp r1
    (cont2, addr2) <- genRExp r2
    addrT <- newTemp
    return (cont1 . cont2 . orCont addrT addr1 addr2, addrT)

genAnd :: RExpT -> SGen (Stream -> Stream, LAddr)
genAnd (A.And loc r1 r2 t) = do
    (cont1, addr1) <- genRExp r1
    (cont2, addr2) <- genRExp r2
    addrT <- newTemp
    return (cont1 . cont2 . andCont addrT addr1 addr2, addrT)

genNot :: RExpT -> SGen (Stream -> Stream, LAddr)
genNot (A.Not loc r t) = do
    (contR, addrR) <- genRExp r
    addrT <- newTemp
    return (contR . notCont addrT addrR, addrT)

genArith :: RExpT -> SGen (Stream -> Stream, LAddr)
genArith (A.Arith loc r1 op r2 t) = do
    (cont1, addr1) <- genRExp r1
    (cont2, addr2) <- genRExp r2
    addrT <- newTemp
    return (cont1 . cont2 . arithCont op addrT addr1 addr2, addrT)

genComp :: RExpT -> SGen (Stream -> Stream, LAddr)
genComp (A.Comp loc r1 op r2 t) = do
    (cont1, addr1) <- genRExp r1
    (cont2, addr2) <- genRExp r2
    addrT <- newTemp
    return (cont1 . cont2 . compCont op addrT addr1 addr2, addrT)

genSign :: RExpT -> SGen (Stream -> Stream, LAddr)
genSign (A.Sign loc op r t) = case op of
    A.Pos _ -> genRExp r
    A.Neg _ -> do
        (contR, addrR) <- genRExp r
        addrT <- newTemp
        return (contR . negCont (overFromTC t) addrT addrR, addrT)

genRefE :: RExpT -> SGen (Stream -> Stream, LAddr)
genRefE (A.RefE loc l t) = do
    (contL, addrL) <- genLExp l
    (extra, addrRef) <- case addrL of
        A _     -> do
            addrT <- newTemp
            return (refCont addrT addrL, addrT)
        Arr _ _ -> addrFromArr addrL
        RefTo _ -> return (id, A $ getAddr addrL)
    return (contL . extra, addrRef)

genRLExp :: RExpT -> SGen (Stream -> Stream, LAddr)
genRLExp (A.RLExp loc l t) = do
    (contL, addrL) <- genLExp l
    case addrL of
        A _ -> return (contL, addrL)
        _   -> do
            addrT <- newTemp
            (extra, addrL') <- case (addrL, t) of
                (Arr b o, TArr _ _) -> do
                    tmp <- newTemp
                    return (arithCont (A.Add t) tmp (A b) (A o), tmp)
                _ -> return (id, addrL)
            return (contL . extra . nilCont addrT addrL' (overFromTC t), addrT)


genArrList :: RExpT -> SGen (Stream -> Stream, LAddr)
genArrList arr@(A.ArrList loc rs t) = do
    addrT <- newTemp
    let ls  = linearize arr
        n   = length ls
        t'  = tctypeOf $ head ls
        sz  = sizeof $ t'
        a   = getAddr addrT
        intToAddr = \n -> ALit . A.LInt $ toInteger n
        helper = \(n, rexp) -> do
            (contR, addrR) <- genRExp rexp
            return $ contR . nilCont (Arr a $ intToAddr $ n * sz) addrR (overFromTC t')
        linearize (A.ArrList _ rs _) = concatMap linearize rs
        linearize r                  = [r]
    cont <- streamCat $ map helper $ zip [0 .. (n-1)] ls
    return (cont, addrT)
        






genCoerce :: RExpT -> SGen (Stream -> Stream, LAddr)
genCoerce (A.Coerce r t) = do
    let from = overFromTC $ tctypeOf r
        to   = overFromTC $ t
    (contR, addrR) <- genRExp r
    addrT <- newTemp
    return (contR . coerceCont from to addrT addrR, addrT)


genFCall :: RExpT -> SGen (Stream -> Stream, LAddr)
genFCall (A.FCall loc ident rs its rt) = do
    contParams <- streamCat $ map genParam $ zip rs its
    addrT <- newTemp
    let f = addrFromId ident
        n = length rs
    return (contParams . fcallCont addrT f n, addrT)


-- Generate continuation for literal expressions
genLit :: RExpT -> SGen (Stream -> Stream, LAddr)
genLit (A.Lit loc lit t) = case lit of
    A.LString s -> do
        addrS <- addSStr s
        return (id, addrS)
    A.LArr ls -> do
        addrT <- newTemp
        let linearize (A.LArr ls) = concatMap linearize ls
            linearize l                  = [l]
            ls' = linearize lit
            n = length ls'
            t' = tctypeOf . head $ ls'
            sz = sizeof t'
            addrS = addrFromInteger . toInteger $ sz
            a   = getAddr addrT
            intToAddr = \n -> ALit . A.LInt $ toInteger n
            helper = \(n, lit) -> do
                (contR, addrR) <- genLit $ A.Lit (A.Loc 0 0) lit t'
                return $ contR . nilCont (Arr a $ intToAddr $ n * sz) addrR (overFromTC t')
        cont <- streamCat $ map helper $ zip [0 .. (n-1)] ls'
        return (cont, addrT)
    _ -> return (id, A . ALit $ lit)






genDeref :: LExpT -> SGen (Stream -> Stream, LAddr)
genDeref (A.Deref l t) = do
    (contL, addrL) <- genLExp l
    (extra, addrRef) <- case addrL of
        A _     -> return (id, newRef addrL)
        Arr _ _ -> do
            addrT <- newTemp
            return (nilCont addrT addrL P, newRef addrT)
        RefTo _ -> do
            addrT <- newTemp
            return (nilCont addrT addrL P, newRef addrT)
    return (contL . extra, addrRef)


genAccess :: LExpT -> SGen (Stream -> Stream, LAddr)
genAccess (A.Access l r t) = do
    (contL, addrL) <- genLExp l
    (contR, addrR) <- genRExp r
    addrT <- newTemp
    let sz = sizeof t
        addrO = case addrL of
            Arr b o -> A o
            _       -> A . ALit . A.LInt $ 0
        addrS = addrFromInteger $ toInteger sz
        contT1 = arithCont (A.Mul TInt) addrT addrS addrR
        contT2 = arithCont (A.Add TInt) addrT addrT addrO
        aL = getAddr addrL
        aT = getAddr addrT
    return (contL . contR . contT1 . contT2, Arr aL aT)


genName :: LExpT -> SGen (Stream -> Stream, LAddr)
genName (A.Name ident@(A.Ident loc name) t) = do
    it <- intentOf $ A.idName ident
    case it of
        A.In      -> caseIn
        A.Out     -> caseOut
        A.InOut   -> caseOut
        A.Ref     -> caseRef
        A.ConstIn -> caseIn
        A.ConstRef -> caseRef
    where
        caseIn = return (id, addrFromId ident)
        caseOut = return (id, A $ AName (name ++ "$local$copy") loc)
        caseRef = return (id, RefTo $ AName name loc)






genStmBlock :: StmT -> SGen (Stream -> Stream)
genStmBlock (A.StmBlock block) = genBlock block

genBlock :: BlockT -> SGen (Stream -> Stream)
genBlock block = do
    let ds = map genDecl $ A.decls block
        ss = map genStm $ A.stms  block
    contDecl <- streamCat ds
    contStms <- streamCat ss
    return $ contDecl . contStms


genStmCall :: StmT -> SGen (Stream -> Stream)
genStmCall (A.StmCall ident rs its) = do
    contParams <- streamCat $ map genParam $ zip rs its
    let f = addrFromId ident
        n = length rs
    return $ contParams . callCont f n

genParam :: (RExpT, Intent) -> SGen (Stream -> Stream)
genParam (r, it) = case it of
    A.In        -> caseIn
    A.Out       -> caseOut
    A.InOut     -> caseOut
    A.Ref       -> caseRef
    A.ConstIn   -> caseIn
    A.ConstRef  -> caseRef
    where
        t = tctypeOf r
        caseIn = do
            (contR, addrR) <- genRExp r
            (extra, addrR') <- case t of
                TArr _ _ -> do
                    tmp <- newTemp
                    contCopy <- (addrR, t) `copyTo` tmp
                    return (contCopy, tmp)
                _ -> return (id, addrR)

            return $ contR . extra . paramCont addrR'
        
        caseRef = let lexp = A.rhsL r in do -- r must be an RLExp
            (contL, addrL) <- genLExp lexp
            (extra, addrL') <- case addrL of
                A _     -> do
                    tmp <- newTemp
                    return (refCont tmp addrL, tmp)
                Arr _ _ -> addrFromArr addrL
                RefTo _ -> return (id, addrL)
            return $ contL . extra . paramCont addrL'

        caseOut = caseRef



genAssign :: StmT -> SGen (Stream -> Stream)
genAssign (A.Assign lexp op rexp) = do
    (contL, addrL) <- genLExp lexp
    (contR, addrR) <- genRExp rexp
    (contOp, addrT) <- helper op addrL addrR
    return $ contL . contR . contOp . nilCont addrL addrT (overFromTC $ tctypeOf lexp)
    where
        helper op l r = case op of
            A.AssignEq _ _  -> return (id, r)
            A.AssignAdd _ t -> do
                addrT <- newTemp
                return (arithCont (A.Add t) addrT l r, addrT)
            A.AssignSub _ t -> do
                addrT <- newTemp
                return (arithCont (A.Sub t) addrT l r, addrT)
            A.AssignMul _ t -> do
                addrT <- newTemp
                return (arithCont (A.Mul t) addrT l r, addrT)
            A.AssignDiv _ t -> do
                addrT <- newTemp
                return (arithCont (A.Div t) addrT l r, addrT)
            A.AssignMod _ -> do
                addrT <- newTemp
                return (arithCont A.Mod     addrT l r, addrT)
            A.AssignPow _ t -> do
                addrT <- newTemp
                return (arithCont (A.Pow t) addrT l r, addrT)




genIf :: StmT -> SGen (Stream -> Stream)
genIf (A.If guard body) = do
    labE <- newLabel "ifFalse"
    contBody <- genStm body
    contGuard <- genGuard guard fall labE 
    return $ contGuard . attachEnd labE contBody


genIfElse :: StmT -> SGen (Stream -> Stream)
genIfElse (A.IfElse guard s1 s2) = do
    labE <- newLabel "ifFalse"
    labN <- newLabel "ifExit"
    cont1 <- genStm s1
    cont2 <- genStm s2
    contG <- genGuard guard fall labE
    return $ contG . cont1 . gotoCont labN . (attachEnd labN $ attachStart labE cont2)


genWhile :: StmT -> SGen (Stream -> Stream)
genWhile (A.While guard stm) = do
    labG <- newLabel "loopGuard"
    labE <- newLabel "loopExit"
    contGuard <- genGuard guard fall labE
    oldBreak <- getBreak
    setBreak labE
    oldContinue <- getContinue
    setContinue labG
    contBody <- genStm stm
    setBreak oldBreak
    setContinue oldContinue
    let contGuard' = attachStart labG contGuard
        contBody'  = attachEnd labE $ attachGuard labG contBody
    return $ contGuard' . contBody'

genDoWhile :: StmT -> SGen (Stream -> Stream)
genDoWhile (A.DoWhile stm guard) = do
    labS <- newLabel "loopBody"
    labG <- newLabel "loopGuard"
    labE <- newLabel "loopExit"
    contGuard <- genGuard guard labS fall
    oldBreak <- getBreak
    setBreak labE
    oldContinue <- getContinue
    setContinue labG
    contBody <- genStm stm
    setBreak oldBreak
    setContinue oldContinue
    let contGuard' = attachEnd labE $ attachStart labG contGuard
        contBody'  = attachStart labS contBody
    return $ contBody' . contGuard'

genFor :: StmT -> SGen (Stream -> Stream)
genFor (A.For ident rng stm) = do
    (contS, addrS) <- genRExp $ A.start rng
    (contE, addrE) <- genRExp $ A.end rng
    labG <- newLabel "forGuard"
    labE <- newLabel "forExit"
    contB <- genStm stm
    let addrI = addrFromId ident
        contAss = nilCont addrI addrS I
        contGuard = ifRelCont addrI (Gt I) addrE labE
        contIncrement = arithCont (A.Add TInt) addrI addrI $ addrFromInteger 1
    return $ contS . contE . contAss . attachStart labG contGuard . (attachEnd labE . attachGuard labG $ contB . contIncrement)


genJmpStm :: StmT -> SGen (Stream -> Stream)
genJmpStm (A.JmpStm jmp) = genJump jmp

genJump :: JumpT -> SGen (Stream -> Stream)
genJump jmp = case jmp of
    A.Return _      -> do
        contCopy <- copyBack
        return $ contCopy . returnCont
    A.ReturnE _ r _ -> do
        contCopy <- copyBack
        (contR, addrR) <- genRExp r
        return $ contCopy . contR . returnECont addrR
    A.Break _ -> do
        break <- getBreak
        return $ gotoCont break
    A.Continue _ -> do
        cont <- getContinue
        return $ gotoCont cont
    where
        copyBack = do
            idF <- getFunction
            mayF <- lookNameF idF
            let idents  = fromMaybe [] mayF
                contsM  = map helper idents
            streamCat contsM
        helper = \(A.Ident loc name, t) -> do
            let addrLocal = A $ AName (name ++ "$local$copy") loc
                addrForm  = A $ AName name loc
            (addrLocal, t) `copyTo` addrForm


                


genGuard :: RExpT -> Label -> Label -> SGen (Stream -> Stream)
genGuard r ifTrue ifFalse = case r of
    A.Comp _ r1 op r2 _ -> do
        (cont1, addr1) <- genRExp r1
        (cont2, addr2) <- genRExp r2
        return $ cont1 . cont2 . fallRel addr1 op addr2

    A.FCall _ _ _ _ _ -> do
        (contR, addrR) <- genRExp r
        return $ contR . fallIf addrR

    A.RLExp _ _ _ -> do
        (contR, addrR) <- genRExp r
        return $ contR . fallIf addrR
    
    A.Or _ r1 r2 _      -> do
        lab   <- newLabel "trueOr"
        let ifTrue' = if ifTrue == fall then lab else ifTrue
        cont1 <- genGuard r1 ifTrue' fall
        cont2 <- genGuard r2 ifTrue  ifFalse
        let extra = if ifTrue == fall then attachStart ifTrue' id else id
        return $ cont1 . cont2 . extra

    A.And _ r1 r2 _     -> do
        lab <- newLabel "falseAnd"
        let ifFalse' = if ifFalse == fall then lab else ifFalse
        cont1 <- genGuard r1 fall ifFalse'
        cont2 <- genGuard r2 fall ifFalse
        let extra = if ifFalse == fall then attachEnd ifFalse' id else id
        return $ cont1 . cont2 . extra

    A.Not _ r _         -> genGuard r ifFalse ifTrue

    A.Lit _ (A.LBool False) _ -> return $ if ifFalse == fall then id else gotoCont ifFalse
    A.Lit _ (A.LBool True)  _ -> return $ if ifTrue == fall then id else gotoCont ifTrue
    
    _ -> return id
    
    where   -- would be nice to abstract this structure
        fallRel addr1 op addr2 = case (ifTrue, ifFalse) of
            (Label "", Label "")    -> id
            (_, Label "")           -> ifRelCont addr1 (toCompOp op) addr2 ifTrue
            (Label "", _)           -> ifRelCont addr1 (opposite $ toCompOp op) addr2 ifFalse
            _                       -> ifRelCont addr1 (toCompOp op) addr2 ifTrue . gotoCont ifFalse

        fallIf addr = case (ifTrue, ifFalse) of
            (Label "", Label "")    -> id
            (_, Label "")           -> ifCont addr ifTrue
            (Label "", _)           -> ifFalseCont addr ifFalse
            _                       -> ifCont addr ifTrue . gotoCont ifFalse