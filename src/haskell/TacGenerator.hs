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
type Intent     = A.Intent
type JumpT      = A.Jump TCType


type Stream = DL.DList TAC          -- The list of TAC instructions

type SGen a = State (Int, Int) a    -- The state is (counter_tempaddr, code)

fall :: Label
fall = Label ""

nilCont :: LAddr -> LAddr -> Over -> Stream -> Stream
nilCont x y ov = mappend $ DL.fromList [Nil x y ov]

refCont :: LAddr -> LAddr -> Stream -> Stream
refCont x y = mappend $ DL.fromList [Ref x y]

binCont :: BinOp -> LAddr -> LAddr -> LAddr -> Stream -> Stream
binCont bop x y z = mappend $ DL.fromList [Bin x y bop z]

unCont :: UnOp -> LAddr -> LAddr -> Stream -> Stream
unCont uop x y = mappend $ DL.fromList [Un x uop y]

orCont :: LAddr -> LAddr -> LAddr -> Stream -> Stream
orCont = binCont Or

andCont :: LAddr -> LAddr -> LAddr -> Stream -> Stream
andCont = binCont And

notCont :: LAddr -> LAddr -> Stream -> Stream
notCont = unCont Not

negCont :: Over -> LAddr -> LAddr -> Stream -> Stream
negCont ov = unCont $ Neg ov

coerceCont :: Over -> LAddr -> LAddr -> Stream -> Stream
coerceCont ov = unCont $ Coerce ov

gotoCont :: Label -> Stream -> Stream
gotoCont lab = mappend $ DL.fromList [Goto lab]

callCont :: LAddr -> Int -> Stream -> Stream
callCont a n = mappend $ DL.fromList [Call a n]

fcallCont :: LAddr -> LAddr -> Int -> Stream -> Stream
fcallCont x f n = mappend $ DL.fromList [FCall x f n]

paramCont :: LAddr -> Stream -> Stream
paramCont x = mappend $ DL.fromList [Par x]

arithCont :: ArithOpT -> LAddr -> LAddr -> LAddr -> Stream -> Stream
arithCont op = binCont $ case op of
    A.Add _ -> Add ov
    A.Sub _ -> Sub ov
    A.Mul _ -> Mul ov
    A.Div _ -> Div ov
    A.Mod   -> Mod
    A.Pow _ -> Pow ov
    where ov = overFromTC $ tctypeOf op

compCont :: CompOpT -> LAddr -> LAddr -> LAddr -> Stream -> Stream
compCont op = binCont . Rel $ toCompOp op

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
    (v, l) <- get
    put (v+1, l)
    return . A . ATemp . ("t"++) $ show v

newRef :: LAddr -> SGen LAddr
newRef = return . RefTo . getAddr



addrFromId :: A.Ident -> LAddr
addrFromId (A.Ident loc name) = A $ AName name loc


addrFromInteger :: Integer -> LAddr
addrFromInteger = A . ALit . A.LInt


labelFromId :: A.Ident -> Label
labelFromId (A.Ident loc name) = Label $ name ++ "@" ++ (show $ A.line loc) ++ "," ++ (show $ A.column loc)


newLabel :: String -> SGen Label
newLabel s = do
    (v, l) <- get
    put (v, l+1)
    return . Label $ s ++ (show l)

attachStart :: Label -> (Stream -> Stream) -> (Stream -> Stream)
attachStart lab cont = instr . cont
    where instr = mappend $ DL.fromList [Lab lab]

attachEnd :: Label -> (Stream -> Stream) -> (Stream -> Stream)
attachEnd lab cont = cont . instr
    where instr = mappend $ DL.fromList [Lab lab]

attachGuard :: Label -> (Stream -> Stream) -> (Stream -> Stream)
attachGuard lab cont = cont . gotoCont lab



genTAC :: ProgramT -> [TAC]
genTAC program = DL.toList $ fst $ runState (genProgram program) (0, 0)


genProgram :: ProgramT -> SGen Stream
genProgram (A.Prog decls) = do
    let contMs = map genDecl decls
    cont <- streamCat contMs
    return $ cont mempty


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
genFDecl (A.FDecl id forms it rt body) = do
    let preamble = labCont $ labelFromId id
    labEnd <- newLabel "endFun"
    contBody <- genBlock body
    let postamble = labCont $ labEnd
    return $ \stream -> mappend stream $ preamble . contBody . postamble $ mempty


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
    addrT <- newTemp
    return (contL . refCont addrT addrL, addrT)

genRLExp :: RExpT -> SGen (Stream -> Stream, LAddr)
genRLExp (A.RLExp loc l t) = do
    (contL, addrL) <- genLExp l
    addrT <- newTemp
    return (contL . nilCont addrT addrL (overFromTC t), addrT)


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
    (contR, addrR) <- genRExp r
    addrT <- newTemp
    return (contR . coerceCont (overFromTC t) addrT addrR, addrT)


genFCall :: RExpT -> SGen (Stream -> Stream, LAddr)
genFCall (A.FCall loc ident rs its rt) = do
    contParams <- streamCat $ map genParam $ zip rs its
    addrT <- newTemp
    let f = addrFromId ident
        n = length rs
    return (contParams . fcallCont addrT f n, addrT)


-- Generate continuation for literal expressions: @TODO Pointers/Arrays/Strings
genLit :: RExpT -> SGen (Stream -> Stream, LAddr)
genLit (A.Lit loc lit t) = case t of
    _ -> return (id, A $ ALit lit)








genDeref :: LExpT -> SGen (Stream -> Stream, LAddr)
genDeref (A.Deref l t) = do
    (contL, addrL) <- genLExp l 
    addrRef <- newRef addrL
    return (contL . id, addrRef)


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
genName (A.Name ident t) = return (id, addr)
    where
        tmp = addrFromId ident
        addr = case t of
            TArr _ _ -> Arr (getAddr tmp) (getAddr $ addrFromInteger 0)
            _        -> tmp






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
genParam (r, it) = do
    (contR, addrR) <- genRExp r
    return $ contR . paramCont addrR



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
    cont1 <- genStm s1
    cont2 <- genStm s2
    contG <- genGuard guard fall labE
    return $ contG . cont1 . attachStart labE cont2


genWhile :: StmT -> SGen (Stream -> Stream)
genWhile (A.While guard stm) = do
    labG <- newLabel "loopGuard"
    labE <- newLabel "loopExit"
    contGuard <- genGuard guard fall labE
    contBody <- genStm stm
    let contGuard' = attachStart labG contGuard
        contBody'  = attachEnd labE $ attachGuard labG contBody
    return $ contGuard' . contBody'

genDoWhile :: StmT -> SGen (Stream -> Stream)
genDoWhile (A.DoWhile stm guard) = do
    labS <- newLabel "loopBody"
    labG <- newLabel "loopGuard"
    labE <- newLabel "loopExit"
    contGuard <- genGuard guard labS fall
    contBody <- genStm stm
    let contGuard' = attachEnd labE $ attachStart labG contGuard
        contBody'  = attachStart labS contBody
    return $ contBody' . contGuard'

genFor :: StmT -> SGen (Stream -> Stream)
genFor (A.For ident rng stm) = do
    contAss <- genStm ass
    contWh  <- genStm wh
    return $ contAss . contWh
    where
        loc = A.Loc 0 0
        s = A.start rng
        t = tctypeOf s
        ass = A.Assign (A.Name ident t) (A.AssignEq loc t) s
        guard = A.Comp loc (A.RLExp loc (A.Name ident t) t) (A.Leq t) (A.end rng) t
        wh = A.While guard stm


genJmpStm :: StmT -> SGen (Stream -> Stream)
genJmpStm (A.JmpStm jmp) = genJump jmp

genJump :: JumpT -> SGen (Stream -> Stream)
genJump jmp = case jmp of
    A.Return _      -> return returnCont
    A.ReturnE _ r _ -> do
        (contR, addrR) <- genRExp r
        return $ contR . returnECont addrR
    _ -> return id


genGuard :: RExpT -> Label -> Label -> SGen (Stream -> Stream)
genGuard r ifTrue ifFalse = case r of
    A.Comp _ r1 op r2 _ -> do
        (cont1, addr1) <- genRExp r1
        (cont2, addr2) <- genRExp r2
        return $ cont1 . cont2 . fallRel addr1 addr2
        where 
            fallRel addr1 addr2 = case (ifTrue, ifFalse) of
                (Label "", Label "")    -> id
                (_, Label "")           -> ifRelCont addr1 (toCompOp op) addr2 ifTrue
                (Label "", _)           -> ifRelCont addr1 (opposite $ toCompOp op) addr2 ifFalse
                _                       -> ifRelCont addr1 (toCompOp op) addr2 ifTrue . gotoCont ifFalse
    
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

    _ -> return id -- Function calls
