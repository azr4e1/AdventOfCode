import unittest
from trebuchet import CalibrationReader, CorrectedCalibrationReader


class TestCalibration(unittest.TestCase):

    def setUp(self):
        self.calibrations = {"1abc2": 12,
                             "pqr3stu8vwx": 38,
                             "a1b2c3d4e5f": 15,
                             "treb7uchet": 77
                             }
        self.total = 142

        self.new_calibration = {
            "two1nine": 29,
            "eightwothree": 83,
            "abcone2threexyz": 13,
            "xtwone3four": 24,
            "4nineeightseven2": 42,
            "zoneight234": 14,
            "7pqrstsixteen": 76
        }
        self.new_total = 281

    def test_lines(self):
        for line, value in self.calibrations.items():
            read_calibration = CalibrationReader(line)
            read_value = read_calibration.get_calibration()
            self.assertEqual(read_value, value)

    def test_total(self):
        total = 0
        for line, value in self.calibrations.items():
            read_calibration = CalibrationReader(line)
            read_value = read_calibration.get_calibration()
            total += read_value

        self.assertEqual(total, self.total)

    def test_newlines(self):
        for line, value in self.new_calibration.items():
            read_calibration = CorrectedCalibrationReader(line)
            read_value = read_calibration.get_calibration()
            self.assertEqual(read_value, value)

    def test_newtotal(self):
        total = 0
        for line, value in self.new_calibration.items():
            read_calibration = CorrectedCalibrationReader(line)
            read_value = read_calibration.get_calibration()
            total += read_value

        self.assertEqual(total, self.new_total)


if __name__ == "__main__":
    unittest.main()
