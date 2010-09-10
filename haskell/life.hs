{-# LANGUAGE EmptyDataDecls #-}

import Test.HUnit

data Cell = Alive | Dead deriving (Show, Eq)

alive :: Cell -> Bool
alive Alive = True
alive Dead = False

class Grid g where
    neighbours :: g -> Cell -> [Cell]

data XGrid
instance Grid XGrid where
    neighbours _ _ = undefined

nextGen :: Grid g => g -> Cell -> Cell
nextGen g c | numAlive <  2 = Dead
            | numAlive == 2 = c
            | numAlive == 3 = Alive
            | numAlive >  3 = Dead
    where numAlive = numberAliveIn (neighbours g c)
          numberAliveIn = length . (filter alive)

{- TESTS -}

data CannedGrid = CannedGrid [Cell]
instance Grid CannedGrid where
    neighbours (CannedGrid ns) _ = ns

noNeighbours = []
twoNeigbours = replicate 2 Alive
threeNeigbours = replicate 3 Alive
fourNeigbours = replicate 4 Alive

test1 = "dies with no neighbours" ~: Dead ~=? nextGen (CannedGrid noNeighbours) Alive

infix 1 `becomes`
becomes = (~?=)
infix 2 `with`
cell `with` neighbours = nextGen (CannedGrid neighbours) cell

tests = [ Alive `with` noNeighbours `becomes` Dead
        , Dead `with` noNeighbours  `becomes` Dead
        , Alive `with` twoNeigbours `becomes` Alive
        , Dead `with` twoNeigbours  `becomes` Dead
        , Alive `with` threeNeigbours `becomes` Alive
        , Dead `with` threeNeigbours  `becomes` Alive
        , Alive `with` fourNeigbours `becomes` Dead
        , Dead `with` fourNeigbours `becomes` Dead
        ]

run = runTestTT $ test (test1:tests)
