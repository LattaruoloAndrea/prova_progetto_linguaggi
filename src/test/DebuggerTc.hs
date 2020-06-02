module Main where

import AbsChapel
import TypeChecker
import TCType
import LexChapel
import ParChapel
import AbsChapel
import TCType
import TCInstances
import Locatable
import Env
import Control.Monad (unless, when)
import qualified ErrM as EM
import qualified ErrT as ET
import PrintChapel

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

-- take a rexp and transform it in abstact syntax

-- makeToken :: String -> [Token]
-- makeToken gr = tokens gr

-- fullRexp :: String -> (EM.Err Program)
-- fullRexp gr = let tok = tokens gr in  --case tokens of
--           pProgram tok

-- createInit :: String -> String -> String
-- createInit typee gr = "var x:"++ typee ++"="++gr++";"

-- easyRexp :: String-> String -> (EM.Err Program)
-- easyRexp typee gr = let tok = tokens (createInit typee gr) in  --case tokens of
--           pProgram tok

-- takeRexp :: (EM.Err Program) -> RExp
-- takeRexp p = case p of
--     EM.Ok g -> takeRexp1 g

-- takeRexp1 :: Program -> RExp
-- takeRexp1 p = case p of
--     Prog (x:xs) -> takeRexp2 x

-- takeRexp2 :: Decl -> RExp
-- takeRexp2 p = case p of
--     VList (x:xs) -> takeRexp3 x

-- takeRexp3 :: VDecl -> RExp
-- takeRexp3 p = case p of
--     Init a b c -> c

-- -- takeRexp (easyRexp "int" "5+6")

-- profRExp :: String -> EM.Err TCType
-- profRExp expr = inferRExp [] (takeRexp (easyRexp "int" expr))
-- --profRExp "5+6"



-- -- ///////////////////////////////////////////////////////////////////////////////////////////////////////


-- -- Parser for non-terminal `X` has name pX

-- -- Generic debugger that abstracts the pipeline
-- debugWith :: ([Token] -> EM.Err a) -> (Env -> a -> EM.Err b) -> (Env -> String -> EM.Err b)
-- debugWith p f = \env s -> case p $ myLexer s of
--     EM.Ok x  -> f env x
--     EM.Bad m -> EM.Bad m

-- debugLExp :: Env -> String -> EM.Err TCType
-- debugLExp = debugWith pLExp inferLExp

-- debugRExp :: Env -> String -> EM.Err TCType
-- debugRExp = debugWith pRExp inferRExp


-- -- debugStm :: Env -> String -> EM.Err Env
-- -- debugStm = debugWith pStm checkStm


-- debugProgram :: String -> EM.Err Env
-- debugProgram = debugWith pProgram checkProgram startEnv


-- COPIA TEST CHAPEL /////////////////////////////////////////////////////////////////////////////////////

type ParseFun a = [Token] -> EM.Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
    EM.Bad s    -> do
        putStrLn "\nParse              Failed...\n"
        -- putStrV v "Tokens:"
        -- putStrV v $ show ts
        putStrLn s
        exitFailure
    EM.Ok  tree -> do
        putStrLn "\nParse Successful!"
        showTree v tree
        case typeCheck tree of
            EM.Bad s -> do
                putStrLn "\nTypeCheck            Failed...\n"
                putStrLn s
                exitFailure
            EM.Ok prog -> do
                putStrLn "\nTypeCheck Successful!"
                showTree v prog
                exitSuccess


showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pProgram
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs