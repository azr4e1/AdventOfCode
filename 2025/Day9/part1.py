from dataclasses import dataclass
from itertools import combinations


@dataclass
class RedTile:
    x: int
    y: int

    def distance(self, other):
        return abs(self.x - other.x+1) * abs(self.y - other.y+1)

    @classmethod
    def tileset(cls, self, other):
        tileset = set()
        min_x, max_x = min(self.x, other.x), max(self.x, other.x)
        min_y, max_y = min(self.y, other.y), max(self.y, other.y)
        for x in range(min_x, max_x+1):
            for y in range(min_y, max_y+1):
                tileset.add(cls(x, y))

        return tileset

    def __hash__(self):
        return hash(f"{self.x}{self.y}")


def parse(input):
    redtiles = []
    for line in input.split("\n"):
        if line.strip() == "":
            continue
        vals = line.split(",")
        redtiles.append(RedTile(*map(int, vals)))

    return redtiles


def area(tile1, tile2):
    return tile1.distance(tile2)


def calculate_distances(redtiles):
    distances = {}
    for box1, box2 in combinations(redtiles, 2):
        distance = box1.distance(box2)
        distances[(box1, box2)] = distance

    sorted_distances = sorted(
        distances.keys(), key=lambda x: distances[x], reverse=True)
    return sorted_distances


with open("./input.txt") as f:
    content = f.read()

redtiles = parse(content)
distances = calculate_distances(redtiles)


print("Biggest Square:", area(*distances[0]))
