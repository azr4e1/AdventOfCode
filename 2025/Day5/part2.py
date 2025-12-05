from __future__ import annotations
from dataclasses import dataclass


@dataclass
class Range:
    start: str
    end: str

    def __post_init__(self):
        start = int(self.start)
        end = int(self.end)
        self.range = range(start, end+1)

    def __iter__(self):
        return iter(self.range)

    def isin(self, id: str):
        start, end = int(self.start), int(self.end)
        if start <= int(id) <= end:
            return True
        return False

    def __len__(self):
        return int(self.end) - int(self.start) + 1

    def merge(self, other: Range) -> tuple[bool, Range, Range]:
        start1, end1 = int(self.start), int(self.end)
        start2, end2 = int(other.start), int(other.end)

        if start1 <= start2 and end1 >= end2:
            new_range = Range(self.start, self.end)
            ok = True
        elif start2 <= start1 and end2 >= end1:
            new_range = Range(other.start, other.end)
            ok = True
        elif start1 <= end2 and start2 <= start1:
            new_range = Range(other.start, self.end)
            ok = True
        elif start2 <= end1 and start1 <= start2:
            new_range = Range(self.start, other.end)
            ok = True
        else:
            new_range = self
            ok = False

        return ok, new_range, other

    def __gt__(self, other):
        if int(self.start) > int(other.start):
            return True
        return False

    def __lt__(self, other):
        if int(self.start) < int(other.start):
            return True
        return False

    def __ge__(self, other):
        if int(self.start) >= int(other.start):
            return True
        return False

    def __le__(self, other):
        if int(self.start) <= int(other.start):
            return True
        return False


def merge(range1: Range, range2: Range) -> tuple[bool, Range, Range]:
    return range1.merge(range2)


def parse_range(ranges_str):
    ranges = []
    for i in ranges_str.split("\n"):
        start_str, end_str = i.strip().split("-")
        start = start_str.strip()
        end = end_str.strip()
        range_int = Range(start, end)
        ranges.append(range_int)

    return ranges


def parse_ids(id_str):
    ids = []
    for i in id_str.split("\n"):
        if i.strip() == "":
            continue
        ids.append(i)

    return ids


def parse(input: str) -> tuple[list[Range], list[str]]:
    ranges_str, id_str = input.split("\n\n")

    ranges = parse_range(ranges_str)
    ids = parse_ids(id_str)

    return ranges, ids


with open("./input.txt") as f:
    content = f.read()
    ranges, ids = parse(content)

fresh_ingredient_ranges = []

# print(sorted(ranges))
r_prev = None
for r in sorted(ranges):
    if r_prev is None:
        r_prev = r
    ok, range1, range2 = merge(r_prev, r)
    r_prev = range1
    if not ok:
        fresh_ingredient_ranges.append(range1)
        r_prev = range2

if ok:
    fresh_ingredient_ranges.append(range1)
else:
    fresh_ingredient_ranges.append(range2)

print("Number of fresh ingredients:", sum(len(r)
      for r in fresh_ingredient_ranges))
