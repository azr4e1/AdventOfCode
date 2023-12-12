import unittest
from cards import Card, Hand
from parser import Parser, JokerParser


class TestCamelCards(unittest.TestCase):
    def setUp(self) -> None:
        self.game = [
            "32T3K 765",
            "T55J5 684",
            "KK677 28",
            "KTJJT 220",
            "QQQJA 483"
        ]
        self.hand_value = [1, 3, 2, 2, 3]
        self.joker_hand_value = [1, 5, 2, 5, 5]
        self.hands = Parser(self.game).parse()
        self.joker_hands = JokerParser(self.game).parse()
        self.total_winnings = 6440
        self.joker_total_winnings = 5905

    def test_handtype(self) -> None:
        hand_type = [hand.hand_value for hand, rank in self.hands]
        self.assertEqual(self.hand_value, hand_type)

    def test_totalwinning(self) -> None:
        # sort by hand
        sorted_hands = sorted(self.hands, key=lambda x: x[0])
        winning = 0
        for index, hand in enumerate(sorted_hands):
            rank = index+1
            value = rank * hand[1]
            winning += value

        self.assertEqual(self.total_winnings, winning)

    def test_joker_handtype(self) -> None:
        hand_type = [hand.hand_value for hand, rank in self.joker_hands]
        self.assertEqual(self.joker_hand_value, hand_type)

    def test_jokertotalwinning(self) -> None:
        # sort by hand
        sorted_hands = sorted(self.joker_hands, key=lambda x: x[0])
        winning = 0
        for index, hand in enumerate(sorted_hands):
            rank = index+1
            value = rank * hand[1]
            winning += value

        self.assertEqual(self.joker_total_winnings, winning)


if __name__ == "__main__":
    unittest.main()
