import unittest
from sequences import OASISequence
from parser import Parser


class TestOasis(unittest.TestCase):
    def setUp(self):
        self.sequences_str = [
            "0 3 6 9 12 15",
            "1 3 6 10 15 21",
            "10 13 16 21 30 45",
        ]
        self.sequences = [
            [0, 3, 6, 9, 12, 15],
            [1, 3, 6, 10, 15, 21],
            [10, 13, 16, 21, 30, 45]
        ]
        self.next_elements = [18, 28, 68]
        self.prev_elements = [-3, 0, 5]

    def test_parser(self):
        parser = Parser(self.sequences_str)
        read_seq = parser.parse()
        seq = [OASISequence(sequence) for sequence in self.sequences]
        self.assertEqual(seq, read_seq)

    def test_next_el(self):
        next_elements = []
        for sequence in self.sequences:
            seq = OASISequence(sequence)
            next_elements.append(seq.next())
        self.assertEqual(next_elements, self.next_elements)

    def test_prev_el(self):
        prev_elements = []
        for sequence in self.sequences:
            seq = OASISequence(sequence)
            prev_elements.append(seq.prev())
        self.assertEqual(prev_elements, self.prev_elements)


if __name__ == "__main__":
    unittest.main()
