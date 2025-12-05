PAPER = "@"


def parse(content: str) -> dict[tuple[int, int], bool]:
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


def get_adjacent_rolls(roll):
    x, y = roll

    return [(x+1, y), (x, y+1), (x-1, y), (x, y-1), (x+1, y+1), (x-1, y+1), (x+1, y-1), (x-1, y-1)]


with open("./input.txt") as f:
    content = f.read()

diagram, rolls = parse(content)
accessible_rolls = 0

for roll in rolls:
    adjacent_rolls = get_adjacent_rolls(roll)
    count_adj = 0
    adj = [diagram.get(r, False) for r in adjacent_rolls]
    if sum(adj) < 4:
        accessible_rolls += 1

print("Number of accessible rolls:", accessible_rolls)
