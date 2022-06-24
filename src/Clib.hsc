{-# LANGUAGE ForeignFunctionInterface #-}
module Clib where

import BCG
import ParseBCG
import Data.Char
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

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

foreign export ccall "how_mono" howMono :: CString -> IO CDouble