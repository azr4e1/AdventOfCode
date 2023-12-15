import unittest
from tiles import Maze, Tile, Tunnel


class TestTile(unittest.TestCase):
    def setUp(self):
        self.maze_str = [
            "..F7.",
            ".FJ|.",
            "SJ.L7",
            "|F--J",
            "LJ...",
        ]
        self.paths = [
            "S|LJF--J7L|7FJFJ",
            "SJFJF7|L7J--FJL|"
        ]
        self.maze = Maze(self.maze_str)
        self.starting_point = Tile(self.maze, 0, 2)
        self.tunnel = Tunnel(self.maze, self.starting_point)
        self.max_distance = 8

    def test_starting_point(self):
        starting_point = self.maze.find_start()
        self.assertEqual(self.starting_point, starting_point)

    def test_path_finding(self):
        path = "".join([tile.tile for tile in self.tunnel.sequence])
        self.assertIn(path, self.paths)

    def test_opposite(self):
        max_distance, _ = self.tunnel.get_polar_opposite()
        self.assertEqual(max_distance, self.max_distance)


class TestArea(unittest.TestCase):
    def setUp(self):
        self.maze_str = [
            "FF7FSF7F7F7F7F7F---7",
            "L|LJ||||||||||||F--J",
            "FL-7LJLJ||||||LJL-77",
            "F--JF--7||LJLJ7F7FJ-",
            "L---JF-JLJ.||-FJLJJ7",
            "|F|F-JF---7F7-L7L|7|",
            "|FFJF7L7F-JF7|JL---7",
            "7-L-JL7||F7|L7F-7F7|",
            "L.L7LFJ|||||FJL7||LJ",
            "L7JLJL-JLJLJL--JLJ.L",
        ]
        self.maze = Maze(self.maze_str)
        self.starting_point = self.maze.find_start()
        self.tunnel = Tunnel(self.maze, self.starting_point)
        self.area = 10

    def test_area(self):
        internal_tiles = self.tunnel.get_internal_tiles()
        area = len(internal_tiles)
        self.assertEqual(area, self.area)


class TestArea2(unittest.TestCase):
    def setUp(self):
        self.maze_str = [
            ".F----7F7F7F7F-7....",
            ".|F--7||||||||FJ....",
            ".||.FJ||||||||L7....",
            "FJL7L7LJLJ||LJ.L-7..",
            "L--J.L7...LJS7F-7L7.",
            "....F-J..F7FJ|L7L7L7",
            "....L7.F7||L7|.L7L7|",
            ".....|FJLJ|FJ|F7|.LJ",
            "....FJL-7.||.||||...",
            "....L---J.LJ.LJLJ...",
        ]
        self.maze = Maze(self.maze_str)
        self.starting_point = self.maze.find_start()
        self.tunnel = Tunnel(self.maze, self.starting_point)
        self.area = 8

    def test_area(self):
        internal_tiles = self.tunnel.get_internal_tiles()
        area = len(internal_tiles)
        self.assertEqual(area, self.area)


if __name__ == "__main__":
    unittest.main()
