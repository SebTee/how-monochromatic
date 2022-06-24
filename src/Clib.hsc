{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Clib
Description : C library
Copyright   : (c) Sebastian Tee, 2022
License     : MIT
Maintainer  : github.com/SebTee

C library for calling the how monochromatic function
-}
module Clib where

import BCG
import ParseBCG
import Foreign.C.String
import Foreign.C.Types

-- | Input a C language string, following the convention defined for 'ParseBCG.parse', 
-- and have the parsed 'BCG.BCG'\'s 'BCG.dist' value returned.
-- 
-- Can also be called as @how_mono(g)@ to follow C function naming conventions.
howMono :: CString -> IO CDouble
howMono cStr = do
  str <- peekCString cStr
  case parse str of
    Right g -> return $ CDouble $ dist g
    Left (e, l) -> case e of
      IncorrectNumberOfParameters -> error $ errMsg "Incorrect number of parameters" l str
      ComplexNumberParseFailure -> error $ errMsg "Failed to parse the complex number" l str
  where
    errMsg m l c = m ++ " on line " ++ show l ++ "\n\n" ++ lines c !! (l - 1)


foreign export ccall howMono :: CString -> IO CDouble
-- The exported function follows the C naming convention
foreign export ccall "how_mono" howMono :: CString -> IO CDouble