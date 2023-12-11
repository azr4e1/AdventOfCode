import unittest
from wait_for_it import Race, Parser


class TestRace(unittest.TestCase):
    def setUp(self):
        self.game = [
            "Time:      7  15   30",
            "Distance:  9  40  200"
        ]
        self.adjusted_game = [
            "Time: 71530",
            "Distance: 940200"
        ]
        self.parsed_game = [
            Race(7, 9),
            Race(15, 40),
            Race(30, 200)
        ]
        self.adjusted_parsed_game = [
            Race(71530, 940200),
        ]
        self.winning_combinations = 288
        self.adjusted_winning_combinations = 71503

    def test_parser(self):
        parser = Parser(self.game)
        read_games = parser.parse()
        self.assertEqual(read_games, self.parsed_game)

    def test_adjusted_parser(self):
        parser = Parser(self.adjusted_game)
        read_games = parser.parse()
        self.assertEqual(read_games, self.adjusted_parsed_game)

    def test_games(self):
        parser = Parser(self.game)
        read_games = parser.parse()
        combinations = 1
        for game in read_games:
            combinations *= len(game.winning_strategies)

        self.assertEqual(combinations, self.winning_combinations)

    def test_adjusted_games(self):
        parser = Parser(self.adjusted_game)
        read_games = parser.parse()
        combinations = 1
        for game in read_games:
            combinations *= len(game.winning_strategies)

        self.assertEqual(combinations, self.adjusted_winning_combinations)


if __name__ == "__main__":
    unittest.main()
