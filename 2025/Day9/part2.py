from dataclasses import dataclass
from itertools import combinations
from math import inf


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


def find_green_tiles(redtiles):
    greentiles = set()
    max_x, max_y = 0, 0
    for rt1, rt2 in zip(redtiles, redtiles[1:]+[redtiles[0]]):
        greentiles.update(RedTile.tileset(rt1, rt2))
        # if rt1.x < min_x:
        #     min_x = rt1.x
        # if rt1.y < min_y:
        #     min_y = rt1.y
        if rt1.x > max_x:
            max_x = rt1.x
        if rt1.y > max_y:
            max_y = rt1.y

    # min_tile, max_tile = RedTile(min_x-2, min_y-2), RedTile(max_x+2, max_y+2)
    # inside = RedTile.tileset(min_tile, max_tile)
    # is_inside = False
    # for x in range(min_x, max_x+1):
    #     for y in range(min_y, max_y+1):
    #         tile = RedTile(x, y)
    #         if tile in greentiles and not is_inside:
    #             is_inside = True
    #         if tile not in greentiles and is_inside:
    #             is_inside = False
    #         if not is_inside:
    #             inside.remove(tile)
    return greentiles, max_x, max_y


with open("./input2.txt") as f:
    content = f.read()

redtiles = parse(content)
distances = calculate_distances(redtiles)


# print("Biggest Square:", area(*distances[0]))
# tileset = RedTile.tileset(*distances[0])
# print(tileset)
# print(len(tileset))

greentiles, max_x, max_y = find_green_tiles(redtiles)

for r1, r2 in distances:
    square = RedTile.tileset(r1, r2)
    if square.issubset(greentiles):
        break

print("Area:", area(r1, r2))

# for y in range(0, max_y+3):
#     print("\n", end="")
#     for x in range(0, max_x+3):
#         tile = RedTile(x, y)
#         if tile in redtiles:
#             print("#", end="")
#         elif tile in greentiles:
#             print("X", end="")
#         else:
#             print(".", end="")
