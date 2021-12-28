-- | Library for handling bi-coloured graphs
module BCG where

import Data.Complex
import qualified Data.IntSet as Set

-- * Data Types

-- | Bi-Coloured Graph
data BCG
    = BCG
    [BCE]      -- ^ List of 'BCE's
    Set.IntSet -- ^ Set of Vertices
    Int        -- ^ Number of different colours
    deriving Show

-- | Bi-Coloured Edge
data BCE = BCE { fromV :: Int            -- ^ From Vertex
               , fromC :: Int            -- ^ From Colour
               , toV   :: Int            -- ^ To Vertex
               , toC   :: Int            -- ^ To Colour
               , w     :: Complex Double -- ^ Weight
} deriving Show

-- * Functions

-- | Get a list of 'BCE's connected to a vertex
getIncidentEdges :: BCG -> Int -> [BCE]
getIncidentEdges (BCG es _ _) v = filter (isIncident v) es

-- | Determines if a vertex is connected to an 'BCE'
isIncident :: Int -> BCE -> Bool
isIncident v BCE{fromV = from, toV = to} = (v == from) || (v == to)

-- | Removes a vertex and the connected 'BCE's from a 'BCG'
removeVertex :: BCG -> Int -> BCG
removeVertex (BCG es vs c) v = BCG (filter (\e -> not $ v `isIncident` e) es) (Set.delete v vs) c

-- | Removes a list of vertices and their connected 'BCE's from a 'BCG'
removeVertices :: BCG -> [Int] -> BCG
removeVertices = foldl removeVertex