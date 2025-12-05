PAPER = "@"


def parse(content: str) -> tuple[dict[tuple[int, int], bool], list[tuple[int, int]]]:
    diagram = {}
    rolls = []
    lines = content.split("\n")
    for i, l in enumerate(lines):
        for j, v in enumerate(l):
            val = True if v == PAPER else False
            diagram[(j, i)] = val
            if val:
                rolls.append((j, i))

    return diagram, rolls


def get_adjacent_rolls(roll) -> list[tuple[int, int]]:
    x, y = roll

    return [(x+1, y), (x, y+1), (x-1, y), (x, y-1), (x+1, y+1), (x-1, y+1), (x+1, y-1), (x-1, y-1)]


with open("./input.txt") as f:
    content = f.read()

diagram, rolls = parse(content)
accessible_rolls = 0

ummovable = False
remaining_rolls = rolls.copy()

while not ummovable:
    ummovable = True
    rem_rolls = []
    for roll in remaining_rolls:
        adjacent_rolls = get_adjacent_rolls(roll)
        count_adj = 0
        adj = [diagram.get(r, False) for r in adjacent_rolls]
        if sum(adj) < 4:
            accessible_rolls += 1
            diagram[roll] = False
            ummovable = False
        else:
            rem_rolls.append(roll)
    remaining_rolls = rem_rolls.copy()

print("Number of accessible rolls:", accessible_rolls)
