module TacGenerator where

import Tac
import Control.Monad.Trans.State
import Control.Monad (foldM)
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
type ArithOpT   = A.ArithOp TCType
type CompOpT    = A.CompOp TCType


type Stream = DL.DList TAC          -- The list of TAC instructions

type SGen a = State (Int, Int) a    -- The state is (counter_tempaddr, code)

nilCont :: LAddr -> LAddr -> Over -> Stream -> Stream
nilCont x y ov = mappend $ DL.fromList [Nil x y ov]

refCont :: LAddr -> LAddr -> Stream -> Stream
refCont x y = mappend $ DL.fromList [Ref x y]

litCont :: LAddr -> A.Literal -> Over -> Stream -> Stream
litCont x lit ov = mappend $ DL.fromList [Lit x lit ov]

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
compCont op = binCont . Rel $ case op of
    A.Lt  _ -> Lt
    A.Leq _ -> Leq
    A.Eq  _ -> Eq
    A.Neq _ -> Neq
    A.Geq _ -> Geq
    A.Gt  _ -> Gt
    $ overFromTC $ tctypeOf op









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


genTAC :: ProgramT -> [TAC]
genTAC program = DL.toList $ fst $ runState (genProgram program) (0, 0)


genProgram :: ProgramT -> SGen Stream
genProgram (A.Prog decls) = do
    let contMs = map genDecl decls
    cont <- streamCat contMs
    return $ cont mempty


genDecl :: DeclT -> SGen (Stream -> Stream)
genDecl decl = case decl of
    A.FDecl _ _ _ _ _ -> return id

    A.VList vs ->
        let contMs = map genVDecl $ reverse vs
        in  streamCat contMs

    A.CList cs ->
        let contMs = map genCDecl $ reverse cs
        in  streamCat contMs


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
    A.ArrList _ _ _   -> return (id, A . ATemp $ "t_not_done_yet")
    A.FCall _ _ _ _   -> return (id, A . ATemp $ "t_not_done_yet")
    A.Lit _ _ _       -> genLit r
    A.Coerce _ _      -> genCoerce r


genLExp :: LExpT -> SGen (Stream -> Stream, LAddr)
genLExp l = case l of
    A.Deref  _ _   -> genDeref l
    A.Access _ _ _ -> return (id, A . ATemp $ "t_not_done_yet")
    A.Name   _ _   -> genName l








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

genCoerce :: RExpT -> SGen (Stream -> Stream, LAddr)
genCoerce (A.Coerce r t) = do
    (contR, addrR) <- genRExp r
    addrT <- newTemp
    return (contR . coerceCont (overFromTC t) addrT addrR, addrT)










genDeref :: LExpT -> SGen (Stream -> Stream, LAddr)
genDeref (A.Deref l t) = do
    (contL, addrL) <- genLExp l
    addrRef <- newRef addrL
    return (contL . id, addrRef)


genName :: LExpT -> SGen (Stream -> Stream, LAddr)
genName (A.Name ident t) = return (id, addrFromId ident)










-- Generate continuation for literal expressions: @TODO Pointers/Arrays/Strings
genLit :: RExpT -> SGen (Stream -> Stream, LAddr)
genLit (A.Lit loc lit t) = 
    case t of
        _           -> do
            addrT <- newTemp
            return (litCont addrT lit $ overFromTC t, addrT)
