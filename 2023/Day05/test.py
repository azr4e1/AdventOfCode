import unittest
from fertilizer import AlmanacParser, BetterAlmanacParser, Range, MultiRange
from test_almanac import ALMANAC


class TestAlmanac(unittest.TestCase):
    def setUp(self):
        with open("test_almanac.txt") as f:
            self.almanac_str = [line.strip() for line in f.readlines()]

        self.almanac = ALMANAC
        self.seeds = [79, 14, 55, 13]
        self.locations = [82, 43, 86, 35]
        self.lowest_location_nr_snd = 46
        self.range_seeds = [Range(79, 14), Range(55, 13)]

    def test_parser(self):
        parser = AlmanacParser(self.almanac_str)
        read_seeds = set(parser.seeds)
        self.assertSetEqual(set(self.seeds), read_seeds)
        self.assertDictEqual(self.almanac, parser.maps)

    def test_location(self):
        parser = AlmanacParser(self.almanac_str)
        read_locations = parser.get_locations()
        self.assertSetEqual(set(read_locations), set(self.locations))

    def test_multirange(self):
        a = Range(0, 20)
        # b = Range(14, 40)
        c = Range(60, 30)

        self.assertEqual(a / c, MultiRange())

    def test_range_seeds(self):
        parser = BetterAlmanacParser(self.almanac_str)
        range_seeds = parser.seeds
        self.assertEqual(range_seeds, self.range_seeds)


if __name__ == "__main__":
    unittest.main()
