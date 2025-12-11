from dataclasses import dataclass
from math import sqrt
from itertools import product


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
    for box1, box2 in product(boxes, boxes):
        distance = box1.distance(box2)
        if distance == 0:
            continue
        distances[(box1, box2)] = distance

    sorted_distances = sorted(distances.keys(), key=lambda x: distances[x])
    return sorted_distances


with open("./input2.txt") as f:
    content = f.read()

boxes = parse(content)
