import unittest
from universe import Universe


class TestUniverse(unittest.TestCase):
    def setUp(self) -> None:
        self.universe = [
            "...#......",
            ".......#..",
            "#.........",
            "..........",
            "......#...",
            ".#........",
            ".........#",
            "..........",
            ".......#..",
            "#...#.....",
        ]
        self.galaxy_nr = 9
        self.galaxy_distances = 374
        self.galaxy_distances_10 = 1030
        self.galaxy_distances_100 = 8410

    def test_nr_galaxies(self):
        read_universe = Universe(self.universe)
        self.assertEqual(self.galaxy_nr, len(read_universe.galaxies))

    def test_distances(self):
        read_universe = Universe(self.universe)
        read_universe.expand()
        distances = read_universe.calculate_distances()
        total_distances = sum(distances.values())
        self.assertEqual(self.galaxy_distances, total_distances)

    def test_distances_10(self):
        read_universe = Universe(self.universe)
        read_universe.expand(10)
        distances = read_universe.calculate_distances()
        total_distances = sum(distances.values())
        self.assertEqual(self.galaxy_distances_10, total_distances)

    def test_distances_100(self):
        read_universe = Universe(self.universe)
        read_universe.expand(100)
        distances = read_universe.calculate_distances()
        total_distances = sum(distances.values())
        self.assertEqual(self.galaxy_distances_100, total_distances)


if __name__ == "__main__":
    unittest.main()
