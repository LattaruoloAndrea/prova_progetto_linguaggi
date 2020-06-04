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


type Stream = DL.DList TAC          -- The list of TAC instructions

type SGen a = State (Int, Int) a    -- The state is (counter_tempaddr, code)

nilCont :: LAddr -> LAddr -> Over -> Stream -> Stream
nilCont x y ov = mappend $ DL.fromList [Nil x y ov]

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
    A.Comp _ _ _ _ _  -> return (id, A . ATemp $ "t_not_done_yet")
    A.Arith _ _ _ _ _ -> return (id, A . ATemp $ "t_not_done_yet")
    A.Sign _ _ _ _    -> return (id, A . ATemp $ "t_not_done_yet")
    A.RefE _ _ _      -> return (id, A . ATemp $ "t_not_done_yet")
    A.RLExp _ _ _     -> return (id, A . ATemp $ "t_not_done_yet")
    A.ArrList _ _ _   -> return (id, A . ATemp $ "t_not_done_yet")
    A.FCall _ _ _ _   -> return (id, A . ATemp $ "t_not_done_yet")
    A.Lit _ _ _       -> genLit r





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


-- Generate continuation for literal expressions: @TODO Pointers/Arrays/Strings
genLit :: RExpT -> SGen (Stream -> Stream, LAddr)
genLit (A.Lit loc lit t) = 
    case t of
        _           -> do
            addrT <- newTemp
            return (litCont addrT lit $ overFromTC t, addrT)
