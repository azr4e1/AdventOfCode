import csv

def create_ranges(elf_range: str):
    range_start, range_end = elf_range.split('-')
    actual_range = set(range(int(range_start), int(range_end)+1))

    return actual_range

# === First Part ===
first_elf = []
second_elf = []
with open("./input.txt") as f:
    reader = csv.reader(f)
    for row in reader:
        first_elf.append(row[0])
        second_elf.append(row[1])

first_elf_ranges = [create_ranges(elf_range) for elf_range in first_elf]
second_elf_ranges = [create_ranges(elf_range) for elf_range in second_elf]

pair_ranges = zip(first_elf_ranges, second_elf_ranges)
is_included = 0

for pair1, pair2 in pair_ranges:
    pair1: set
    pair2: set
    if pair1.issubset(pair2) or pair2.issubset(pair1):
        is_included += 1


print("Number of pairs where one range is included in the other is:")
print(is_included)

# === Second Part ===
pair_ranges = list(zip(first_elf_ranges, second_elf_ranges))
total_pairs = len(pair_ranges)
is_disjoint = 0
for pair1, pair2 in pair_ranges:
    pair1: set
    pair2: set
    if pair1.isdisjoint(pair2):
        is_disjoint += 1

overlapping = total_pairs - is_disjoint

print("Number of pairs where ranges overlap is:")
print(overlapping)
