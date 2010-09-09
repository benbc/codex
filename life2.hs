{-# LANGUAGE EmptyDataDecls #-}

import Test.HUnit

data Cell = Alive | Dead deriving (Show, Eq)

alive :: Cell -> Bool
alive Alive = True
alive Dead = False

data Grid
type Neighbours = Grid -> Cell -> [Cell]
neighbours :: Neighbours
neighbours = undefined

type NextGen = Grid -> Cell -> Cell
nextGen :: NextGen
nextGen = generalNextGen neighbours

generalNextGen :: Neighbours -> NextGen
generalNextGen neighbours g c | numAlive <  2 = Dead
                              | numAlive == 2 = c
                              | numAlive == 3 = Alive
                              | numAlive >  3 = Dead
    where numAlive = numberAliveIn (neighbours g c)
          numberAliveIn = length . (filter alive)

{- TESTS -}

cannedNeighbourNextGen :: [Cell] -> NextGen
cannedNeighbourNextGen neighbours = generalNextGen (\ _ _ -> neighbours)

noNeighbours = []
twoNeigbours = replicate 2 Alive
threeNeigbours = replicate 3 Alive
fourNeigbours = replicate 4 Alive

test1 = "no neighbours dies" ~: Dead ~=? cannedNeighbourNextGen noNeighbours undefined Alive

infix 1 `becomes`
becomes = (~?=)
infix 2 `with`
cell `with` neighbours = cannedNeighbourNextGen neighbours undefined cell

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
