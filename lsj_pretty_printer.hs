module Main where

import Data.List

data Prog
    = Prog String [Stat]
    deriving(Show, Eq)

data Stat
    = SStat String
    | MStat String [Stat]
    deriving(Show, Eq)

addIndention :: String -> String
addIndention s = r
    where
        r = intercalate "\n" lsList
        lsList = map ("    "++) ls
        ls = lines s

progToString :: Prog -> String
progToString (Prog n ss)
    = n ++ "\n" ++ stats
        where stats = addIndention $ intercalate "\n" (map statToString ss)

statToString :: Stat -> String
statToString (SStat n) = n
statToString (MStat n ss)
    = n ++ "\n" ++ stats
        where stats = addIndention $ intercalate "\n" (map statToString ss)

main :: IO ()
main = putStr $ progToString (Prog "prog1" [SStat "stat1", MStat "stat2" [SStat "stat3", SStat "stat4"]])
