from dataclasses import dataclass


@dataclass
class Range:
    start: str
    end: str

    def __post_init__(self):
        start = int(self.start)
        end = int(self.end)
        self.range = [str(i+start) for i in range(end - start + 1)]

    def __iter__(self):
        return iter(self.range)


def parse(input: str) -> list[Range]:
    ranges_str = input.split(",")
    ranges = []
    for i in ranges_str:
        start_str, end_str = i.strip().split("-")
        start = start_str.strip()
        end = end_str.strip()
        range_int = Range(start, end)
        ranges.append(range_int)

    return ranges


def is_valid(id: str) -> bool:
    if id.startswith("0"):
        return False
    if len(id) % 2 != 0:
        return True
    half = len(id) // 2
    if id[:half] == id[half:]:
        return False
    return True


with open("./input.txt") as f:
    content = f.read()

ranges = parse(content)

invalid_ids = []
for r in ranges:
    for id in r:
        if not is_valid(id):
            invalid_ids.append(int(id))

print("Sum of invalid ids:", sum(invalid_ids))
