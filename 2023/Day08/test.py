import unittest
from parser import Parser
from navigator import Navigator, GhostlyNavigator


class TestNetwork(unittest.TestCase):
    def setUp(self):
        self.network_str = [
            "LLR",
            "",
            "AAA = (BBB, BBB)",
            "BBB = (AAA, ZZZ)",
            "ZZZ = (ZZZ, ZZZ)",
        ]
        self.directions = ["L", "L", "R"]
        self.network = {
            "AAA": {"L": "BBB", "R": "BBB"},
            "BBB": {"L": "AAA", "R": "ZZZ"},
            "ZZZ": {"L": "ZZZ", "R": "ZZZ"}
        }
        self.parser = Parser(self.network_str)
        self.steps = 6
        self.ghostly_network_str = [
            "LR",
            "",
            "11A = (11B, XXX)",
            "11B = (XXX, 11Z)",
            "11Z = (11B, XXX)",
            "22A = (22B, XXX)",
            "22B = (22C, 22C)",
            "22C = (22Z, 22Z)",
            "22Z = (22B, 22B)",
            "XXX = (XXX, XXX)"
        ]
        parser = Parser(self.ghostly_network_str)
        self.ghostly_instructions = parser.instructions
        self.ghostly_network = parser.network
        self.ghostly_steps = 6

    def test_directions_parsing(self):
        self.assertEqual(self.parser.instructions, self.directions)

    def test_network_parsing(self):
        self.assertDictEqual(self.parser.network, self.network)

    def test_steps(self):
        navigator = Navigator(self.directions, self.network)
        navigator.navigate()
        steps = len(navigator.steps)-1
        self.assertEqual(steps, self.steps)

    def test_ghostlynavigator(self):
        navigator = GhostlyNavigator(self.ghostly_instructions, self.ghostly_network)
        navigator.navigate()
        self.assertEqual(navigator.steps, self.ghostly_steps)


if __name__ == "__main__":
    unittest.main()
