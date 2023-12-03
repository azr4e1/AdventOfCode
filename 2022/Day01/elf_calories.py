# import the calories, separated by elf
from collections import defaultdict
elf_calories = defaultdict(list)
with open("./input.txt") as f:
    elf = 1
    for line in f:
        if line == "\n":
            elf += 1
            continue
        elf_calories[elf].append(int(line))

total_elf_calories = []
for elf in elf_calories:
    total_elf_calories.append(sum(elf_calories[elf]))

print("Total calories of elf carrying most calories:")
print(max(total_elf_calories))

total_elf_calories.sort(reverse=True)

print("Total calories of three elves carrying most calories:")
print(sum(total_elf_calories[:3]))
