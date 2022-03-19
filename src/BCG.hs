-- | Library for handling bi-coloured graphs
module BCG where

import Data.Complex
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

-- * Data Types

-- | Bi-Colored Graph
data BCG
    = BCG
    [BCE]      -- ^ List of 'BCE's
    IntSet.IntSet     -- ^ Set of Vertices
    Int        -- ^ Number of different colours
    deriving Show

-- | Bi-Coloured Edge
data BCE = BCE { fromV :: Int            -- ^ From Vertex
               , fromC :: Int            -- ^ From Colour
               , toV   :: Int            -- ^ To Vertex
               , toC   :: Int            -- ^ To Colour
               , w     :: Complex Double -- ^ Weight
} deriving Show

-- | Inherited Vertex Coloring
type IVC = Set.Set (Int, Int)

-- | Perfect Matching
type PM = BCG

-- * Constructors

-- | A 'BCG' with no edges or vertices and the number of different colors is 0
empty :: BCG
empty = BCG [] IntSet.empty 0

-- * Functions

-- | Return a value between 0 and 1 representing a 'BCG'\'s distance to the monochromatic
-- \[dist(G) = \begin{cases} 
--     0 & G \text{ has no perfect matchings} \\
--     \frac{\left|\displaystyle\sum_{c_i \in G,\ c_i\ \text{is monochromatic}} w(c_i)\right|^2}{d\cdot\left(\displaystyle\sum_{c_i \in G} |w(c_i)|^2\right)} & \text{Otherwise}
-- \end{cases}\]
-- Where \(c_i\) is an 'IVC' on \(G\), \(d\) is the number of different colors on \(G\) and \(w(c_i)\) is the colouring weight of \(c_i\).
dist :: BCG -> Double
dist g
    | null pms = 0 -- return 0 in the case of no perfect matchings
    | otherwise = case g of
        (BCG _ _ d) -> (magnitude (sum $ map coloringWeight monochromaticIvcs) ^ 2) / (fromIntegral d * norm)
    where
        pms = enumeratePM g
        ivcs = groupByIvc pms
        monochromaticIvcs = filter (\(pm:_) -> isMonochromatic pm) ivcs
        norm = sum $ map (\c -> magnitude (coloringWeight c) ^ 2) ivcs

-- | Get a list of perfect matchings on a 'BCG'
-- The perfect matchings are represented by the 'BCG' data type
enumeratePM :: BCG -> [BCG]
enumeratePM (BCG (e:es) vs c) = (if isPM then [BCG [e] vs c] else map addMatchingBack $ enumeratePM newSubG ) ++ enumeratePM (BCG es vs c)
    where
        isPM = case e of BCE{fromV = from, toV = to} -> IntSet.fromList [from, to] == vs
        newSubG = removeEdgeAndIncidentVertices (BCG (e:es) vs c) e
        addMatchingBack (BCG mes mvs _) = case e of BCE{fromV = from, toV = to} -> BCG (e:mes) (IntSet.insert from $ IntSet.insert to mvs) c
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
removeVertex (BCG es vs c) v = BCG (filter (\e -> not $ v `isIncident` e) es) (IntSet.delete v vs) c

-- | Removes a list of vertices and their connected 'BCE's from a 'BCG'
removeVertices :: BCG -> [Int] -> BCG
removeVertices = foldl removeVertex

-- | Checks if all edges on a 'PM' are monochromatic and have the same color
isMonochromatic :: PM -> Bool
isMonochromatic (BCG es _ _) = foldr ((&&) . (\c -> c == head colors)) True $ tail colors
    where
        getColors BCE{fromC = from, toC = to} = [from, to]
        colors = concatMap getColors es

-- | Takes a list of 'PM's with the same 'IVC' and returns the 'IVC'\'s weight
coloringWeight :: [PM] -> Complex Double
coloringWeight ((BCG es _ _):gs) = edgeWeightProduct es + coloringWeight gs
    where
        edgeWeightProduct (BCE{w = w}:es') = w * edgeWeightProduct es'
        edgeWeightProduct _ = 1
coloringWeight _ = 0

-- | Get the 'IVC' of a 'PM'
pmsIvc :: PM -> IVC
pmsIvc (BCG es _ _) = Set.fromList $ concatMap (\BCE{fromV = fv, fromC = fc, toV = tv, toC = tc} -> [(fv, fc), (tv, tc)]) es

-- | Groups a list of 'PM'\'s by 'IVC'
groupByIvc :: [PM] -> [[PM]]
groupByIvc (p:ps) = (p : sameIvc) : groupByIvc diffIvc
    where
        ivc = pmsIvc p
        hasSameIvc p' = pmsIvc p' == ivc
        sameIvc = filter hasSameIvc ps
        diffIvc = filter (not . hasSameIvc) ps
groupByIvc _ = []

-- | Check if 2 vertices are adjacent
isAdjacent :: BCG -> Int -> Int -> Bool
isAdjacent g from to = IntSet.member to $ getAdjacent g from

-- | Get a set of adjacent nodes
getAdjacent :: BCG -> Int -> IntSet.IntSet
getAdjacent (BCG es _ _) v = IntSet.delete v $ IntSet.fromList $ concat $ filter (elem v) $ map (\BCE{fromV = fv, toV = tv} -> [fv, tv]) es

-- | get list of vertices connected to a vertex on a 'BCG'
getConnected :: BCG -> Int -> IntSet.IntSet
getConnected g v = IntSet.delete v $ gc (IntSet.singleton v) g v
    where
        gc :: IntSet.IntSet -> BCG -> Int -> IntSet.IntSet
        gc r g v = foldr (IntSet.union . gc nr g) IntSet.empty (IntSet.toList yetToVisit)
            where
                adj = getAdjacent g v
                yetToVisit = IntSet.filter (\e -> not $ IntSet.member e r) adj
                nr = adj `IntSet.union` r

-- | Determine if a given 'BCG' is a fully connected graph
isConnectedGraph :: BCG -> Bool
isConnectedGraph g = case g of 
    BCG _ vs _ -> IntSet.delete start vs == getConnected g start
        where start = head $ IntSet.toList vs