import Test.HUnit hiding (State)

data State = Alive | Dead deriving (Eq)
data Cell = Cell State Position

alive, dead :: Cell -> Bool
alive (Cell Alive _) = True
alive (Cell Dead _) = False
dead = not . alive

data Position = Position
class Grid g where
    neighbours :: g -> Position -> [Cell]

nextGen :: Grid g => g -> Cell -> Cell
nextGen grid (Cell state position) = Cell state' position
    where state' | numAlive <  2 = Dead
                 | numAlive == 2 = state
                 | numAlive == 3 = Alive
                 | numAlive >  3 = Dead
          numAlive = numberAliveIn (neighbours grid position)
          numberAliveIn = length . (filter alive)

data XGrid = XGrid
instance Grid XGrid where
    neighbours g p = undefined

{- TESTS -}

data CannedGrid = CannedGrid [Cell]
instance Grid CannedGrid where
    neighbours (CannedGrid ns) _ = ns

anAlive = Cell Alive undefined
aDead = Cell Dead undefined

noNeighbours = []
twoNeigbours = replicate 2 anAlive
threeNeigbours = replicate 3 anAlive
fourNeigbours = replicate 4 anAlive

test1 = dead successor ~? "dies with no neighbours"
    where successor = nextGen (CannedGrid noNeighbours) anAlive

infix 1 `becomes`
becomes cell pred = pred cell ~? ""
infix 2 `with`
cell `with` neighbours = nextGen (CannedGrid neighbours) cell

tests = [ anAlive `with` noNeighbours `becomes` dead
        , aDead `with` noNeighbours  `becomes` dead
        , anAlive `with` twoNeigbours `becomes` alive
        , aDead `with` twoNeigbours  `becomes` dead
        , anAlive `with` threeNeigbours `becomes` alive
        , aDead `with` threeNeigbours  `becomes` alive
        , anAlive `with` fourNeigbours `becomes` dead
        , aDead `with` fourNeigbours `becomes` dead
        ]

run = runTestTT $ test (test1:tests)
