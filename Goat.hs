-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Goat Compiler                     --
--  Implemented by Shjie Liu, Wenqing Xue, Minjian Chen  --
-----------------------------------------------------------

module Main (main) where

import GoatParser (ast)
import GoatCompiler (initial, getCode)
import GoatFormat (progToStr)
import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode(..))

-- Used to distinguish the output
data Task
  = Pprint | Compile | Parse
  deriving (Eq, Show)

-- Main function for Goat compiler
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
                              putStr $ getCode (initial tree)
              Left   err -> do
                              putStr "Parse error at "
                              print err
                              exitWith (ExitFailure 3)
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

