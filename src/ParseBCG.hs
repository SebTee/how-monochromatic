-- | Library for parsing strings into 'BCG.BCG'
module ParseBCG (
    parse,
    ParseException (IncorrectNumberOfParameters, ComplexNumberParseFailure)
) where

import BCG
import Data.Complex
import qualified Data.Map as Map
import qualified Data.IntSet as Set
import Text.Read (readMaybe)

-- | Parse a string into 'BCG.BCG'
parse :: String -> Either (ParseException, Int) BCG
parse s = case parsePossibleBCETokens $ map parseLine $ filter (not . null) $ map words $ lines s of
  Right tokens -> Right $ parseBCETokens (P Map.empty Map.empty) tokens
  Left e -> Left e

-- | Exceptions returned by the parser
data ParseException
  = IncorrectNumberOfParameters -- ^ The line has an incorrect number of parameters
  | ComplexNumberParseFailure   -- ^ The real or imaginary part of the complex number couldn't be parsed into 'Prelude.Double'
  deriving (Show)

-- Internal from this point on

-- Map of names and corresponding 'Int' IDs
type NameIdMap = (Map.Map String Int)

-- | Data to be stored throughout the parsing process
data Persistent
  = P
  NameIdMap -- ^ Map of Vertex names to their Int IDs
  NameIdMap -- ^ Map of Color names to their Int IDs

-- | Similar to 'BCG.BCE' but uses names instead of IDs
data BCEToken = BCEToken { fromV :: String         -- ^ From Vertex
                         , fromC :: String         -- ^ From Colour
                         , toV   :: String         -- ^ To Vertex
                         , toC   :: String         -- ^ To Colour
                         , w     :: Complex Double -- ^ Weight
} deriving Show

-- | Parse a line into a 'BCEToken'
parseLine :: [String] -> Either ParseException BCEToken
parseLine [fv, fc, tv, tc, rw, iw] = case parseComplexNumber rw iw of
  Just w -> Right $ BCEToken fv fc tv tc w
  Nothing  -> Left ComplexNumberParseFailure
parseLine _ = Left IncorrectNumberOfParameters

-- | Parse possible 'BCEToken's into 'BCEToken's or return an exception
parsePossibleBCETokens :: [Either ParseException BCEToken] -> Either (ParseException, Int) [BCEToken]
parsePossibleBCETokens (ept:pts) = case ept of
  Right t -> case parsePossibleBCETokens pts of
    Right ts            -> Right $ t:ts
    Left (e, l)         -> Left (e, l + 1)
  Left e                -> Left (e, 1)
parsePossibleBCETokens _ = Right []

-- | Parse 'BCETokens' into a 'BCG'
parseBCETokens :: Persistent -> [BCEToken] -> BCG
parseBCETokens (P v c) ((BCEToken fv fc tv tc w):ts) = case parseBCETokens (P nnv nnc) ts of
  BCG bces vertices colors -> BCG (BCE fvId fcId tvId tcId w:bces) vertices colors
  where
    ((nv, fvId), (nc, fcId)) = (getId v fv, getId c fc)
    ((nnv, tvId), (nnc, tcId)) = (getId nv tv, getId nc tc)
parseBCETokens (P v c) _ = BCG [] (Set.fromList $ map snd $ Map.toList v) $ Map.size c

-- | Parse two strings into real number parts of a complex number
parseComplexNumber :: String -> String -> Maybe (Complex Double)
parseComplexNumber sr si = case (readMaybe sr :: Maybe Double, readMaybe si :: Maybe Double) of
  (Just r, Just i) -> Just $ r :+ i
  _ -> Nothing

-- | Gets the corresponding ID for a name and adds a new one if one does not exist
getId :: NameIdMap -> String -> (NameIdMap, Int)
getId m k = case Map.lookup k m of
  Just id -> (m, id)
  Nothing -> (Map.insert k newId m, newId)
  where newId = Map.size m