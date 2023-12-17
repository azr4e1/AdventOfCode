import re
from itertools import combinations_with_replacement, permutations

BROKEN_SPRING = "#"
WORKING_SPRING = "."
UNKNOWN_SPRING = "?"


class ReportLine:
    def __init__(self, line: str):
        self._parse(line)

    def _parse(self, line):
        records, broken_groups_str = line.split(" ")
        broken_groups = [int(x) for x in broken_groups_str.split(",")]

        self.records = records
        self.broken_groups = broken_groups

    def _get_all_arrangements(self) -> set[tuple[int, ...]]:
        question_marks = self.records.count("?")
        combinations = combinations_with_replacement([BROKEN_SPRING,
                                                      WORKING_SPRING],
                                                     question_marks)
        all_arrangements = set()
        for comb in combinations:
            perms = set(permutations(comb))
            all_arrangements.update(perms)

        return all_arrangements

    def _build_record(self, arrangement: tuple[int, ...]) -> str:
        index = 0
        new_record = ""
        for char in self.records:
            if char == UNKNOWN_SPRING:
                new_record += arrangement[index]
                index += 1
            else:
                new_record += char

        # cheekily add . to start and end to help
        # out the regex
        return "." + new_record + "."

    def _get_regex(self) -> re.Pattern:
        groups = ["#"*group for group in self.broken_groups]
        partial_pattern = r"\.+".join(groups)
        pattern = r"^\.+" + partial_pattern + r"\.+$"

        return re.compile(pattern)

    def get_arrangements(self) -> set[str]:
        all_arrangements = self._get_all_arrangements()
        pattern = self._get_regex()
        valid_arrangements = set()
        for arrangement in all_arrangements:
            new_record = self._build_record(arrangement)
            if pattern.match(new_record):
                valid_arrangements.add(new_record)

        return valid_arrangements
