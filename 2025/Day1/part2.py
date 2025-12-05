

def _rotate(input: str):
    direction = input[0]
    distance = int(input[1:])
    if direction == "L":
        return -1 * distance
    return distance


def rotate(current: int, input: str):
    clicks = _rotate(input)
    zero_clicks = 0
    for i in range(abs(clicks)):
        current = (current + clicks/abs(clicks)) % 100
        if current == 0:
            zero_clicks += 1
    return current, zero_clicks


rotations = []
with open("./input.txt") as f:
    for line in f:
        rotations.append(line)

current_pos = 50

zero_counter = 0
for rotation in rotations:
    current_pos, zero_clicks = rotate(current_pos, rotation)
    zero_counter += zero_clicks

print("Password:", zero_counter)
