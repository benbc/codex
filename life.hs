import Test.HUnit

data Cell = Alive | Dead deriving (Show, Eq)

alive :: Cell -> Bool
alive Alive = True
alive Dead = False

type Neighbourhood = [Cell]
numberAliveIn :: Neighbourhood -> Int
numberAliveIn = length . (filter alive)

nextGen :: Grid g => g -> Cell -> Cell
nextGen g c = f c
    where numAlive = numberAliveIn (neighbours g c)
          f _ | numAlive <  2 = Dead
          f c | numAlive == 2 = c
          f _ | numAlive == 3 = Alive
          f _ | numAlive >  3 = Dead

class Grid g where
    neighbours :: g -> Cell -> Neighbourhood

data CannedGrid = CannedGrid Neighbourhood
instance Grid CannedGrid where
    neighbours (CannedGrid ns) _ = ns

noNeighbours = CannedGrid []
twoNeigbours = CannedGrid (replicate 2 Alive)
threeNeigbours = CannedGrid (replicate 3 Alive)
fourNeigbours = CannedGrid (replicate 4 Alive)

infix 1 `becomes`
becomes = (~?=)

infix 2 `with`
cell `with` grid = (nextGen grid cell)

tests = test [ Alive `with` noNeighbours `becomes` Dead
             , Dead `with` noNeighbours  `becomes` Dead
             , Alive `with` twoNeigbours `becomes` Alive
             , Dead `with` twoNeigbours  `becomes` Dead
             , Alive `with` threeNeigbours `becomes` Alive
             , Dead `with` threeNeigbours  `becomes` Alive
             , Alive `with` fourNeigbours `becomes` Dead
             , Dead `with` fourNeigbours `becomes` Dead
             ]

main = runTestTT tests

-- TOOD
-- * try refactoring to make Neighbourhood a type class
-- * can you do tell-don't-ask in FP?
--   (e.g. pass Neighbourhood a function which it calls with numAlive)
