---------------------------------------------------------
-- Programming Language Implementation COMP90045 Project1
-- Implemented by Shjie Liu, Wenqing Xue, Minjian Chen
---------------------------------------------------------

module Main (main) where

import GoatParser (ast)
import GoatCompiler (test, getCode)
import GoatFormat (progToStr)
import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode(..))

data Task
  = Pprint | Compile | Parse
  deriving (Eq, Show)

main :: IO()
main
  = do
      progname <- getProgName
      args <- getArgs
      task <- checkArgs progname args
      case task of
        Compile
          -> do
            let [filename] = args
            -- Read file name
            input <- readFile filename
            let output = ast input
            case output of
              Right tree -> do
                              -- putStr $ show (test tree)
                              -- putStr "\n\n\n\n\n\n"
                              putStr $ getCode (test tree)
              Left   err -> do
                              putStr "Parse error at "
                              print err
                              exitWith (ExitFailure 2)
        Parse
          -> do
                let [_, filename] = args
                -- Read file name
                input <- readFile filename
                let output = ast input
                case output of
                  Right tree -> do
                                  putStrLn (show tree)
                  Left   err -> do
                                  putStr "Parse error at "
                                  print err
                                  exitWith (ExitFailure 2)
        Pprint
          -> do
                let [_, filename] = args
                -- Read file name
                input <- readFile filename
                let output = ast input
                case output of
                  Right tree -> putStr $ progToStr tree
                  Left   err -> do
                                  putStr "Parse error at "
                                  print err
                                  exitWith (ExitFailure 2)

-- Code is provided by Harald Sondergaard
checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]
  = do
      putStrLn ("Missing filename")
      exitWith (ExitFailure 1)
checkArgs _ [filename]
  = return Compile
checkArgs _ ["-p", filename]
  = return Pprint
checkArgs _ ["-a", filename]
  = return Parse
checkArgs progname _
  = do
      putStrLn ("Usage: " ++ progname ++ " [-p] filename")
      exitWith (ExitFailure 1)

