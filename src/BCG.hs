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

-- * Constructors

-- | A 'BCG' with no edges or vertices and the number of different colors is 0
empty :: BCG
empty = BCG [] Set.empty 0

-- * Functions

-- | Get a list of perfect matchings on a 'BCG'
-- The perfect matchings are represented by the 'BCG' data type
enumeratePM :: BCG -> [BCG]
enumeratePM (BCG (e:es) vs c) = (if isPM then [BCG [e] vs c] else map addMatchingBack $ enumeratePM newSubG ) ++ enumeratePM (BCG es vs c)
    where 
        isPM = case e of BCE{fromV = from, toV = to} -> Set.fromList [from, to] == vs
        newSubG = removeEdgeAndIncidentVertices (BCG (e:es) vs c) e
        addMatchingBack (BCG mes mvs _) = case e of BCE{fromV = from, toV = to} -> BCG (e:mes) (Set.insert from $ Set.insert to mvs) c 
enumeratePM _ = []

-- | Get a list of 'BCE's connected to a vertex
getIncidentEdges :: BCG -> Int -> [BCE]
getIncidentEdges (BCG es _ _) v = filter (isIncident v) es

-- | Determines if a vertex is connected to an 'BCE'
isIncident :: Int -> BCE -> Bool
isIncident v BCE{fromV = from, toV = to} = (v == from) || (v == to)

-- | Removes a 'BCE' from a 'BCG' and the vertices it was incident to
removeEdgeAndIncidentVertices :: BCG -> BCE -> BCG
removeEdgeAndIncidentVertices g BCE{fromV = from, toV = to} = removeVertices g [from, to]

-- | Removes a vertex and the connected 'BCE's from a 'BCG'
removeVertex :: BCG -> Int -> BCG
removeVertex (BCG es vs c) v = BCG (filter (\e -> not $ v `isIncident` e) es) (Set.delete v vs) c

-- | Removes a list of vertices and their connected 'BCE's from a 'BCG'
removeVertices :: BCG -> [Int] -> BCG
removeVertices = foldl removeVertex