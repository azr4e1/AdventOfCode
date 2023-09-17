import string
from collections import defaultdict

# === First Part ===
def divider(items):
    n_items = len(items)
    first_container = items[:int(n_items/2)]
    second_container = items[int(n_items/2):]

    return first_container, second_container


def group_identifier(input):
    group_sacks = defaultdict(list)
    with open("./input.txt") as f:
        group = 0
        elf = 0
        for line in f:
            if (elf % 3) == 0:
                group += 1
            group_sacks[group].append(line.strip())
            elf += 1

    return group_sacks


def badge_finder(group):
    set1 = set(group[0])
    set2 = set(group[1])
    set3 = set(group[2])

    comm_el = set1.intersection(set2)
    comm_el.intersection_update(set3)

    return comm_el.pop()


priorities = {}
priority = 1
for letter in string.ascii_letters:
    priorities[letter] = priority
    priority += 1


# import list of items
list_of_items = []
with open("./input.txt") as f:
    for line in f:
        list_of_items.append(line.strip())

items_in_both = defaultdict(list)
for rucksack in list_of_items:
    first_container, second_container = divider(rucksack)
    for item in set(first_container):
        if item in second_container:
            items_in_both[rucksack].append(item)

items_in_both_all_rucksacks = []
for item in items_in_both.values():
    items_in_both_all_rucksacks += item

total_priority = sum([priorities[item] for item in items_in_both_all_rucksacks])
print("Total priority of items in both containers is:")
print(total_priority)

# === Second Part ===
group_sacks = group_identifier("./input.txt")

badges = []
for group in group_sacks.values():
    badge = badge_finder(group)
    badges.append(badge)

print("Sum of priorities of badges is:")
sum_of_badges = sum([priorities[badge] for badge in badges])
print(sum_of_badges)
