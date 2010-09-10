class Cell:
    def __init__(self, grid, position):
        self.grid = grid
        self._position = position
    def is_dead(self):
        return not self.is_alive()
    def _num_living_neighbours(self):
        return len(filter(lambda n: n.is_alive(), self._neighbours()))
    def _neighbours(self):
        return self.grid.neighbours(self)

class Alive(Cell):
    def next_gen(self):
        if self._num_living_neighbours() not in [2, 3]:
            self.__class__ = Dead
        return self
    def is_alive(self):
        return True

class Dead(Cell):
    def next_gen(self):
        if self._num_living_neighbours() == 3:
            self.__class__ = Alive
        return self
    def is_alive(self):
        return False

def with_neighbours(num):
    class StubGrid:
        def neighbours(self, cell):
            return [Alive(self, None) for n in range(0, num)] + [Dead(self, None) for n in range(0, 8-num)]
    return StubGrid()

def test_dies_with_no_neighbours():
    assert Alive(with_neighbours(0), None).next_gen().is_dead()

def make_test(num_alive, start, expected):
    name = 'test_%s_with_%s_neighbours_%s' % (start.__name__, num_alive, expected)
    def test():
        next = start(with_neighbours(num_alive), None).next_gen()
        assert getattr(next, expected)()
    globals()[name] = test

[make_test(*spec) for spec in [[0, Alive, 'is_dead'],
                               [0, Dead,  'is_dead'],
                               [1, Alive, 'is_dead'],
                               [1, Dead,  'is_dead'],
                               [2, Alive, 'is_alive'],
                               [2, Dead,  'is_dead'],
                               [3, Alive, 'is_alive'],
                               [3, Dead,  'is_alive'],
                               [4, Alive, 'is_dead'],
                               [4, Dead,  'is_dead'],
                               [5, Alive, 'is_dead'],
                               [5, Dead,  'is_dead'],
                               [8, Alive, 'is_dead'],
                               [8, Dead,  'is_dead']]]
