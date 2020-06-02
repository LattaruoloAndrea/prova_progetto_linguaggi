module TacGenerator where

import Tac
import Control.Monad.Trans.State
import Control.Monad (foldM)
import Control.Applicative (liftA2)
import Locatable
import qualified AbsChapel as A
import qualified Data.DList as DL

type Stream = DL.DList TAC  -- The list of TAC instructions

type SGen a = State (Int, Int) a    -- The state is (counter_tempaddr, code)


newTemp :: SGen LAddr
newTemp = do
    (v, l) <- get
    put (v+1, l)
    return . A . ATemp . ("t"++) $ show v

addrFromId :: A.Ident -> LAddr
addrFromId (A.Ident loc name) = A $ AName name loc

genNil :: LAddr -> LAddr -> Stream -> Stream
genNil l r = mappend $ DL.fromList [Nil l r]


genTAC :: A.Program -> [TAC]
genTAC program = DL.toList $ fst $ runState (genProgram program) (0, 0)


genProgram :: A.Program -> SGen Stream
genProgram (A.Prog decls) = foldM (flip genDecl) mempty decls


genDecl :: A.Decl -> Stream -> SGen Stream
genDecl decl = \stream -> case decl of
    A.FDecl _ _ _ _ _ -> return stream

    A.VList _ -> return stream

    A.CList cs -> do
        let contMs = map genCDecl cs
        cont <- foldr1 (liftA2 (.)) contMs
        return $ (stream `mappend`) $ cont $ DL.fromList []


genCDecl :: A.CDecl -> SGen (Stream -> Stream)
genCDecl (A.CDecl id t r) = do
    (contR, addrR) <- genRExp r
    let addrC    = addrFromId id
        contTac  = genNil addrC addrR
        contDecl = \stream -> (stream `mappend`) $ contTac . contR $ mempty
    return contDecl



genRExp :: A.RExp -> SGen (Stream -> Stream, LAddr)
genRExp r = do
    tmp <- newTemp
    return ((mempty `mappend`), tmp)