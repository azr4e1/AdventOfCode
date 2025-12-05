rotations = []


def _rotate(input: str):
    direction = input[0]
    distance = int(input[1:])
    if direction == "L":
        return -1 * distance
    return distance


def rotate(current: int, input: str):
    return (current + _rotate(input)) % 100


with open("./input.txt") as f:
    for line in f:
        rotations.append(line)

current_pos = 50

zero_counter = 0
for rotation in rotations:
    current_pos = rotate(current_pos, rotation)
    if current_pos == 0:
        zero_counter += 1

print("Password:", zero_counter)
