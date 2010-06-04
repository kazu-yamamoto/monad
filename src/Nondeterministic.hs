module Non where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Sequence as S

data Node = A | B | C | D | E | F deriving (Eq, Ord, Show)

type Network = M.Map Node [Node]

network :: Network
network = M.fromList [
    (A, [B, C])
  , (B, [A, C, D])
  , (C, [A, B, E])
  , (D, [B, E, F])
  , (E, [C, D, F])
  , (F, [E])
  ]

neighbors :: Node -> Network -> [Node]
neighbors n g = fromJust $ M.lookup n g

noLoop :: Node -> S.Seq Node -> Bool
noLoop x sq = isNothing $ S.elemIndexL x sq

addTo :: Node -> S.Seq Node -> S.Seq Node
addTo = flip (S.|>)

currentNode :: S.Seq Node -> Node
currentNode path = case S.viewr path of
    _ S.:> x -> x
    _        -> error "currentNode"

pathFind :: Network -> Node -> S.Seq Node -> [S.Seq Node]
pathFind net goal path = do
    x <- neighbors (currentNode path) net
    guard $ noLoop x path
    let path' = x `addTo` path
    if x == goal
        then return path'
        else pathFind net goal path'
