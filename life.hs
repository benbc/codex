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
neighbourless = CannedGrid []
twoNeighboured = CannedGrid (replicate 2 Alive)
threeNeighboured = CannedGrid (replicate 3 Alive)
fourNeighboured = CannedGrid (replicate 4 Alive)

infix 1 `shouldBecome`
shouldBecome :: Grid g => (g, Cell) -> Cell -> Test
(grid, cell) `shouldBecome` result = (nextGen grid cell) ~?= result

tests = test [ (neighbourless, Alive) `shouldBecome` Dead
             , (neighbourless, Dead)  `shouldBecome` Dead
             , (twoNeighboured, Alive) `shouldBecome` Alive
             , (twoNeighboured, Dead)  `shouldBecome` Dead
             , (threeNeighboured, Alive) `shouldBecome` Alive
             , (threeNeighboured, Dead)  `shouldBecome` Alive
             , (fourNeighboured, Alive) `shouldBecome` Dead
             , (fourNeighboured, Dead) `shouldBecome` Dead
             ]

main = runTestTT tests

-- TOOD
-- * try refactoring to make Neighbourhood a type class
-- * can you do tell-don't-ask in FP?
--   (e.g. pass Neighbourhood a function which it calls with numAlive)
