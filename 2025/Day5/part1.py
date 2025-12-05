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

fresh_ids = 0
for id in ids:
    for r in ranges:
        if r.isin(id):
            fresh_ids += 1
            break


print("Number of fresh ingredients:", fresh_ids)
