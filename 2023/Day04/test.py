import unittest
from scratchcards import CardParser, Scratchcard, CorrectedScratchcard, Game


class TestScratchards(unittest.TestCase):
    def setUp(self) -> None:
        self.games = [
            "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
            "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
            "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
            "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11",
        ]
        self.scratchcards = [
            Scratchcard([41, 48, 83, 86, 17], [83, 86, 6, 31, 17, 9, 48, 53]),
            Scratchcard([13, 32, 20, 16, 61], [61, 30, 68, 82, 17, 32, 24, 19]),
            Scratchcard([1, 21, 53, 59, 44], [69, 82, 63, 72, 16, 21, 14, 1]),
            Scratchcard([41, 92, 73, 84, 69], [59, 84, 76, 51, 58, 5, 54, 83]),
            Scratchcard([87, 83, 26, 28, 32], [88, 30, 70, 12, 93, 22, 82, 36]),
            Scratchcard([31, 18, 13, 56, 72], [74, 77, 10, 23, 35, 67, 36, 11])
        ]
        self.values = [
            8,
            2,
            2,
            1,
            0,
            0
        ]
        self.total_scratchcards = 30

    def test_parsing(self):
        for line, card in zip(self.games, self.scratchcards):
            reader = CardParser(line)
            read_card = reader.parse()
            self.assertEqual(read_card, card)

    def test_value(self):
        for line, value in zip(self.games, self.values):
            reader = CardParser(line)
            read_card = reader.parse()
            self.assertEqual(value, read_card.value)

    def test_newrules(self):
        cards = []
        for line in self.games:
            reader = CardParser(line)
            read_card = CorrectedScratchcard.convert(reader.parse())
            cards.append(read_card)
        game = Game(cards)
        game.process_deck()
        read_total = game.total
        self.assertEqual(self.total_scratchcards, read_total)


if __name__ == "__main__":
    unittest.main()
