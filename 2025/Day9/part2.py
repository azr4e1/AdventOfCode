from dataclasses import dataclass
from itertools import combinations
from tqdm import tqdm


class Queue:
    def __init__(self, *args):
        self.queue = list(args)
        self.quick_check = set(args)

    def add(self, el):
        self.queue.append(el)
        self.quick_check.add(el)

    def pop(self):
        if self.queue:
            el = self.queue[0]
            self.queue = self.queue[1:]
            self.quick_check.remove(el)

            return el
        return None

    def isempty(self) -> bool:
        return len(self.queue) == 0

    def __str__(self):
        return " ".join([str(x) for x in self.queue])

    def __repr__(self):
        return self.__str__()

    def __contains__(self, el):
        return el in self.quick_check


@dataclass
class RedTile:
    x: int
    y: int

    def distance(self, other):
        return (abs(self.x - other.x)+1) * (abs(self.y - other.y)+1)

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

    def get_proximity(self):
        proxy = set()
        for x in [self.x-1, self.x, self.x+1]:
            for y in [self.y-1, self.y, self.y+1]:
                tile = self.__class__(x, y)
                if tile == self:
                    continue
                proxy.add(tile)
        return proxy


def is_in_square(el, rt1, rt2):
    min_x, min_y = min([rt1.x, rt2.x]), min([rt1.y, rt2.y])
    max_x, max_y = max([rt1.x, rt2.x]), max([rt1.y, rt2.y])

    if min_x < el.x < max_x and min_y < el.y < max_y:
        return True
    return False


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


def find_perimeter(redtiles):
    greentiles = set()
    for rt1, rt2 in zip(redtiles, redtiles[1:]+[redtiles[0]]):
        greentiles.update(RedTile.tileset(rt1, rt2))

    return greentiles


def expand_area(perimeter, el: RedTile, progress=False):
    if el in perimeter:
        return None
    max_x, max_y = max(t.x for t in perimeter) + \
        2, max(t.y for t in perimeter) + 2

    def is_in_grid(el) -> bool:
        if 0 <= el.x <= max_x and 0 <= el.y <= max_y:
            return True
        return False

    queue = Queue(el)

    portion = set()
    if progress:
        pbar = tqdm(total=max_x*max_y)
    while not queue.isempty():
        new_el = queue.pop()
        # print(new_el)
        portion.add(new_el)
        if progress:
            pbar.update()
        proxy = new_el.get_proximity()
        for i in proxy:
            if is_in_grid(i) and i not in perimeter and i not in portion and i not in queue:
                queue.add(i)

    pbar.close()

    return portion


def is_square_inside(perimeter, rt1, rt2):
    min_x, min_y = min([rt1.x, rt2.x]), min([rt1.y, rt2.y])
    max_x, max_y = max([rt1.x, rt2.x]), max([rt1.y, rt2.y])

    start_tile = RedTile(min_x+1, min_y+1)

    def is_in_grid(el) -> bool:
        if min_x < el.x < max_x and min_y < el.y < max_y:
            return True
        return False

    queue = Queue(start_tile)

    portion = set()
    # pbar = tqdm(total=area(rt1, rt2))
    while not queue.isempty():
        new_el = queue.pop()
        # pbar.update()
        portion.add(new_el)
        proxy = new_el.get_proximity()
        for i in proxy:
            if is_in_grid(i) and i not in perimeter and i not in portion and i not in queue:
                queue.add(i)

    # pbar.close()
    square_perimeter = abs(rt1.x - rt2.x)*2 + abs(rt1.y-rt2.y)*2
    square_area = area(rt1, rt2)

    inside_area = square_area - square_perimeter
    discovered_area = len(portion)

    return discovered_area == inside_area


def cross_perim(perimeter, rt1, rt2):
    square_perim = find_perimeter([rt1, rt2])
    common_els = square_perim.intersection(perimeter)

    for el in common_els:
        proxies = el.get_proximity()
        for el2 in proxies:
            if is_in_square(el2, rt1, rt2):
                return True

    return False


with open("./input2.txt") as f:
    content = f.read()

redtiles = parse(content)
print("Calculating distances...")
distances = calculate_distances(redtiles)


# print("Biggest Square:", area(*distances[0]))
# tileset = RedTile.tileset(*distances[0])
# print(tileset)
# print(len(tileset))

print("Finding perimeter...")
perimeter = find_perimeter(redtiles)

# get the top left tile
# min_y = min(t.y for t in redtiles)
# top_tiles = {t for t in redtiles if t.y == min_y}
# min_x = min(t.x for t in top_tiles)
# inside_tile = RedTile(min_x+1, min_y+1)

# print(inside_tile)


# print("Expanding area...")
# inside = expand_area(perimeter, inside_tile, progress=True)
# inside.update(perimeter)

for rt1, rt2 in tqdm(distances):
    # square = RedTile.tileset(rt1, rt2)
    # if square.issubset(inside):
    #     break
    if cross_perim(perimeter, rt1, rt2):
        continue
    if is_square_inside(perimeter, rt1, rt2):
        break


# for y in range(0, 9+1):
#     for x in range(0, 14+1):
#         rt = RedTile(x, y)
#         if rt in square:
#             print("O", end="")
#         elif rt in redtiles:
#             print("#", end="")
#         elif rt in inside:
#             print("X", end="")
#         else:
#             print(".", end="")
#     print()

print("Area is", area(rt1, rt2))
