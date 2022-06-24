module Main where

import BCG
import ParseBCG
import System.Exit

main :: IO ()
main = do
    contents <- getContents
    case parse contents of
        Right g -> print $ dist g
        Left (e, l) -> case e of
            IncorrectNumberOfParameters -> die $ errMsg "Incorrect number of parameters" l contents
            ComplexNumberParseFailure -> die $ errMsg "Failed to parse the complex number" l contents
    where
        errMsg m l c = m ++ " on line " ++ show l ++ "\n\n" ++ lines c !! (l - 1)
