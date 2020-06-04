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
type RExpT      = A.RExp TCType


type Stream = DL.DList TAC          -- The list of TAC instructions

type SGen a = State (Int, Int) a    -- The state is (counter_tempaddr, code)


-- compose a list of transformation to obtain the "concatanation" transformation
streamCat :: [SGen (Stream -> Stream)] -> SGen (Stream -> Stream)
streamCat = foldr (liftA2 (.)) (return id)

newTemp :: SGen LAddr
newTemp = do
    (v, l) <- get
    put (v+1, l)
    return . A . ATemp . ("t"++) $ show v

addrFromId :: A.Ident -> LAddr
addrFromId (A.Ident loc name) = A $ AName name loc

genNil :: LAddr -> LAddr -> Stream -> Stream
genNil l r = mappend $ DL.fromList [Nil l r]


genTAC :: ProgramT -> [TAC]
genTAC program = DL.toList $ fst $ runState (genProgram program) (0, 0)


genProgram :: ProgramT -> SGen Stream
genProgram (A.Prog decls) = do
    let contMs = map genDecl decls
    cont <- foldl (liftA2 (.)) (return id) contMs
    return $ cont mempty


genDecl :: DeclT -> SGen (Stream -> Stream)
genDecl decl = case decl of
    A.FDecl _ _ _ _ _ -> return id

    A.VList _ -> return id

    A.CList cs ->
        let contMs = map genCDecl $ reverse cs
        in  streamCat contMs


genCDecl :: CDeclT -> SGen (Stream -> Stream)
genCDecl (A.CDecl id t r) = do
    (contR, addrR) <- genRExp r
    let addrC    = addrFromId id
        contTac  = genNil addrC addrR
        contDecl = contTac . contR
    return contDecl



genRExp :: RExpT -> SGen (Stream -> Stream, LAddr)
genRExp r = do
    tmp <- newTemp
    return (mappend mempty, tmp)   -- mempty will be substituted by code for r