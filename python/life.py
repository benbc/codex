class Cell:
    def __init__(self, grid):
        self.grid = grid
    def is_dead(self):
        return not self.is_alive()
    def _num_living_neighbours(self):
        return len(filter(lambda n: n.is_alive(), self._neighbours()))
    def _neighbours(self):
        return self.grid.neighbours(self)

class Alive(Cell):
    def next_gen(self):
        if self._num_living_neighbours() == 2:
            return Alive(self.grid)
        return Dead(self.grid)
    def is_alive(self):
        return True

class Dead(Cell):
    def next_gen(self):
        return Dead(self.grid)
    def is_alive(self):
        return False

def with_neighbours(num):
    class StubGrid:
        def neighbours(self, cell):
            return [Alive(self) for n in range(0, num)] + [Dead(self) for n in range(0, 8-num)]
    return StubGrid()

def make_test(num_alive, start, expected):
    name = 'test_%s_with_%s_neighbours_%s' % (start.__name__, num_alive, expected)
    def test():
        next = start(with_neighbours(num_alive)).next_gen()
        assert getattr(next, expected)()
    globals()[name] = test

[make_test(*spec) for spec in [[0, Alive, 'is_dead'],
                               [0, Dead,  'is_dead'],
                               [1, Alive, 'is_dead'],
                               [1, Dead,  'is_dead'],
                               [2, Alive, 'is_alive'],
                               [2, Dead,  'is_dead'],
                               ]]
