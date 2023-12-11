import unittest
from cards import Card, Hand
from parser import Parser


class TestCamelCards(unittest.TestCase):
    def setUp(self):
        self.game = [
            "32T3K 765",
            "T55J5 684",
            "KK677 28",
            "KTJJT 220",
            "QQQJA 483"
        ]
        self.hand_value = [1, 3, 2, 2, 3]
        self.hands = Parser(self.game).parse()
        self.total_winnings = 6440

    def test_handtype(self):
        hand_type = [hand.hand_value for hand, rank in self.hands]
        self.assertEqual(self.hand_value, hand_type)

    def test_totalwinning(self):
        # sort by hand
        sorted_hands = sorted(self.hands, key=lambda x: x[0])
        winning = 0
        for index, hand in enumerate(sorted_hands):
            rank = index+1
            value = rank * hand[1]
            winning += value

        self.assertEqual(self.total_winnings, winning)


if __name__ == "__main__":
    unittest.main()
