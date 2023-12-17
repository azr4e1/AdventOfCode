import unittest
from utils import ReportLine


class TestSprings(unittest.TestCase):
    def setUp(self):
        self.actual_record = [
            "#.#.### 1,1,3",
            ".#...#....###. 1,1,3",
            ".#.###.#.###### 1,3,1,6",
            "####.#...#... 4,1,1",
            "#....######..#####. 1,6,5",
            ".###.##....# 3,2,1",
        ]
        self.damaged_record = [
            "???.### 1,1,3",
            ".??..??...?##. 1,1,3",
            "?#?#?#?#?#?#?#? 1,3,1,6",
            "????.#...#... 4,1,1",
            "????.######..#####. 1,6,5",
            "?###???????? 3,2,1",
        ]
        self.arrangements = [1, 4, 1, 1, 4, 10]
        self.total_arrangements = 21

    def test_arrangements(self):
        for index, line in enumerate(self.damaged_record):
            report = ReportLine(line)
            arrangements = report.get_arrangements()
            self.assertEqual(self.arrangements[index], len(arrangements))

    def test_total_combinations(self):
        read_combinations = 0
        for line in self.damaged_record:
            report = ReportLine(line)
            arrangements = report.get_arrangements()
            read_combinations += len(arrangements)

        self.assertEqual(self.total_arrangements, read_combinations)


if __name__ == "__main__":
    unittest.main()
