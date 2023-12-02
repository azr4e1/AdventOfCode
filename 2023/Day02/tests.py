import unittest
from cube_conundrum import GameReader, GameDraw, GameConfiguration, Game


class TestGames(unittest.TestCase):
    def setUp(self) -> None:
        self.test_lines = [
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
        ]
        self.draws = [
            [GameDraw(4, 0, 3), GameDraw(1, 2, 6), GameDraw(0, 2, 0)],
            [GameDraw(0, 2, 1), GameDraw(1, 3, 4), GameDraw(0, 1, 1)],
            [GameDraw(20, 8, 6), GameDraw(4, 13, 5), GameDraw(1, 5, 0)],
            [GameDraw(3, 1, 6), GameDraw(6, 3, 0), GameDraw(14, 3, 15)],
            [GameDraw(6, 3, 1), GameDraw(1, 2, 2)]
        ]
        self.configuration: GameConfiguration = {'red': 12,
                                                 'green': 13,
                                                 'blue': 14}
        self.possible_games = [
            True,
            True,
            False,
            False,
            True
        ]

        self.minimum_cubes = [
            {'red': 4, 'green': 2, 'blue': 6},
            {'red': 1, 'green': 3, 'blue': 4},
            {'red': 20, 'green': 13, 'blue': 6},
            {'red': 14, 'green': 3, 'blue': 15},
            {'red': 6, 'green': 3, 'blue': 2}
        ]

    def test_reader(self) -> None:
        for line, draw in zip(self.test_lines, self.draws):
            single_game = GameReader(line)

            self.assertEqual(single_game.draws, draw)

    def test_gamecheck(self) -> None:
        for line, possible in zip(self.test_lines, self.possible_games):
            reader = GameReader(line)
            game = Game(self.configuration)
            read_possible = game.check(reader.draws)

            self.assertEqual(possible, read_possible)

    def test_minconfig(self) -> None:
        for line, min_config in zip(self.test_lines, self.minimum_cubes):
            reader = GameReader(line)
            read_min_config = reader.valid_configuration()
            self.assertEqual(Game(min_config), read_min_config)


if __name__ == "__main__":
    unittest.main()
