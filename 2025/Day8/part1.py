from dataclasses import dataclass
from math import sqrt
from itertools import combinations


@dataclass
class JunctionBox:
    x: int
    y: int
    z: int

    def distance(self, other):
        x = self.x - other.x
        y = self.y - other.y
        z = self.z - other.z

        distance = sqrt(x**2 + y**2 + z**2)
        return distance

    def __hash__(self):
        return hash(f"{self.x}{self.y}{self.z}")


def parse(input):
    boxes = []
    for line in input.split("\n"):
        if line.strip() == "":
            continue
        vals = line.split(",")
        boxes.append(JunctionBox(*map(int, vals)))

    return boxes


def calculate_distances(boxes):
    distances = {}
    for box1, box2 in combinations(boxes, 2):
        distance = box1.distance(box2)
        if distance == 0:
            continue
        distances[(box1, box2)] = distance

    sorted_distances = sorted(distances.keys(), key=lambda x: distances[x])
    return sorted_distances


with open("./input.txt") as f:
    content = f.read()

boxes = parse(content)
distances = calculate_distances(boxes)

links = [set([box]) for box in boxes]
for i in distances[:11]:
    mini_set = set(i)
    intersections = mini_set.copy()
    for l in links.copy():
        if len(mini_set.intersection(l)) > 0:
            intersections.update(l)
            index = links.index(l)
            del links[index]
    links.append(intersections)

lengths = sorted([len(s) for s in links], reverse=True)
print("Product is:", lengths[0] * lengths[1] * lengths[2])
print("Product is:", lengths)
