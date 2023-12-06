import unittest
from fertilizer import AlmanacParser
from test_almanac import ALMANAC


class TestAlmanac(unittest.TestCase):
    def setUp(self):
        with open("test_almanac.txt") as f:
            self.almanac_str = [line.strip() for line in f.readlines()]

        self.almanac = ALMANAC
        self.seeds = [79, 14, 55, 13]
        self.locations = [82, 43, 86, 35]
        self.lowest_location_nr_snd = 46

    def test_parser(self):
        parser = AlmanacParser(self.almanac_str)
        read_seeds = set(parser.seeds)
        self.assertSetEqual(set(self.seeds), read_seeds)
        self.assertDictEqual(self.almanac, parser.maps)

    def test_location(self):
        parser = AlmanacParser(self.almanac_str)
        read_locations = parser.get_locations()
        self.assertSetEqual(set(read_locations), set(self.locations))


if __name__ == "__main__":
    unittest.main()
