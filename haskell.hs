data Cell = Alive | Dead deriving (Show)

alive :: Cell -> Bool
alive Alive = True
alive Dead = False

type Neighbourhood = [Cell]
numberAliveIn :: Neighbourhood -> Int
numberAliveIn = length . (filter alive)

nextState :: Grid a => a -> Cell -> Cell
nextState g c = f c
    where numAlive = numberAliveIn (neighbours g c)
          f _ | numAlive <  2 = Dead
          f c | numAlive == 2 = c
          f _ | numAlive == 3 = Alive
          f _ | numAlive >  3 = Dead

class Grid a where
    neighbours :: a -> Cell -> Neighbourhood

data CannedGrid = CannedGrid Neighbourhood
instance Grid CannedGrid where
    neighbours (CannedGrid ns) _ = ns
neighbourless = CannedGrid []
twoNeighboured = CannedGrid (replicate 2 Alive)
threeNeighboured = CannedGrid (replicate 3 Alive)

main = do print (nextState neighbourless Alive)
          print (nextState neighbourless Dead)
          print (nextState threeNeighboured Alive)
          print (nextState threeNeighboured Dead)
          print (nextState twoNeighboured Alive)
          print (nextState twoNeighboured Dead)
