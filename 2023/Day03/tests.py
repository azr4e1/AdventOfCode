import unittest
from great_ratios import SchematicsParser, AdvancedSchematicsParser


class TestEngine(unittest.TestCase):
    def setUp(self) -> None:
        self.schematics = [
            "467..114..",
            "...*......",
            "..35..633.",
            "......#...",
            "617*......",
            ".....+.58.",
            "..592.....",
            "......755.",
            "...$.*....",
            ".664.598..",
        ]
        self.symbols = [
            ('*', (3, 1)),
            ('#', (6, 3)),
            ('*', (3, 4)),
            ('+', (5, 5)),
            ('$', (3, 8)),
            ('*', (5, 8)),
        ]

        self.engine_parts_numbers = [
            467,
            35, 633,
            617,
            592,
            755,
            664,
            598
        ]

        self.gears = {
            (3, 1): 16345,
            (5, 8): 451490
        }

    def test_schematics_symbols(self):
        reader = SchematicsParser(self.schematics)
        read_symbols = reader.get_symbols()
        self.assertEqual(set(self.symbols), set(read_symbols))

    def test_schematics_parts(self):
        reader = SchematicsParser(self.schematics)
        read_engine_parts_numbers = [engine.value for engine in reader.get_parts()
                                     if engine.is_engine_part]
        self.assertEqual(set(self.engine_parts_numbers),
                         set(read_engine_parts_numbers))

    def test_gears(self):
        reader = AdvancedSchematicsParser(self.schematics)
        gears = reader.find_gears()
        self.assertDictEqual(self.gears, gears)


if __name__ == "__main__":
    unittest.main()
