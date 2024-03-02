import re

BROKEN_SPRING = "#"
WORKING_SPRING = "."
UNKNOWN_SPRING = "?"

FOLD_FACTOR = 5


class ReportLine:
    def __init__(self, line: str):
        self._parse(line)

    def _parse(self, line):
        records, broken_groups_str = line.split(" ")
        broken_groups = [int(x) for x in broken_groups_str.split(",")]

        self.records = records
        self.broken_groups = broken_groups

    def _get_all_arrangements(self) -> set[tuple[int, ...]]:
        question_marks = self.records.count(UNKNOWN_SPRING)
        broken_springs_nr = self.records.count(BROKEN_SPRING)
        all_broken_springs = sum(self.broken_groups)
        missing_broken_springs = all_broken_springs - broken_springs_nr
        working_spring_len = question_marks - missing_broken_springs
        broken_springs = BROKEN_SPRING * missing_broken_springs
        working_spings = WORKING_SPRING * working_spring_len
        all_arrangements = permutations_with_repetition(broken_springs, working_spings)

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
                valid_arrangements.add(new_record[1:-1])

        return valid_arrangements


class UnfoldedReportLine(ReportLine):
    def __init__(self, line):
        super().__init__(line)
        self.records = UNKNOWN_SPRING.join([self.records]*FOLD_FACTOR)
        self.broken_groups *= FOLD_FACTOR


def permutations_with_repetition(set1: str, set2: str):
    """
    https://jjj.de/fxt/fxtbook.pdf @ Permutations of a multiset
    """
    if not set1:
        return {set2}
    if not set2:
        return {set1}
    rest_of_permutations1 = permutations_with_repetition(set1,
                                                         set2[1:])
    rest_of_permutations2 = permutations_with_repetition(set1[1:],
                                                         set2)
    new_set1 = {set1[0] + perm for perm in rest_of_permutations2}
    new_set2 = {set2[0] + perm for perm in rest_of_permutations1}

    new_set = new_set1.union(new_set2)

    return new_set
