from collections import defaultdict


class Position:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __hash__(self):
        return hash(f"{self.x}{self.y}")

    def __eq__(self, other):
        return (self.x == other.x) and (self.y == other.y)

    def __str__(self):
        return f"Position(x={self.x}, y={self.y})"

    def __repr__(self):
        return self.__str__()


def fall_beams(beams):
    for beam, val in beams.copy().items():
        del beams[beam]
        beam.y += 1
        beams[beam] = val


def split_beam(beam: Position):
    return Position(beam.x-1, beam.y), Position(beam.x+1, beam.y)


def parse(input: str):
    beams = defaultdict(int)
    lines = input.split("\n")
    initial_line = lines[0]
    beams[Position(initial_line.index("S"), 0)] = 1

    return beams.copy(), lines


def get_beams(beams, diagram):
    for y, line in enumerate(diagram):
        for x, el in enumerate(line):
            if el == "^":
                splitter = Position(x, y)
                if splitter in beams:
                    beam_val = beams[splitter]
                    del beams[splitter]
                    beam1, beam2 = split_beam(splitter)
                    beams[beam1] += beam_val
                    beams[beam2] += beam_val
        fall_beams(beams)

    return beams


def get_beams_recursive(beam_x, diagram):
    number_of_beams = 0
    if not diagram:
        return 1
    line = diagram[0]
    was_split = False
    for x, el in enumerate(line):
        if el == "^" and x == beam_x:
            beam1, beam2 = x-1, x+1
            no_beams1 = get_beams_recursive(beam1, diagram[1:])
            no_beams2 = get_beams_recursive(beam2, diagram[1:])
            number_of_beams += no_beams1 + no_beams2
            was_split = True
    if not was_split:
        number_of_beams += get_beams_recursive(beam_x, diagram[1:])

    return number_of_beams


with open("./input.txt") as f:
    content = f.read()

beams, lines = parse(content)

beam = list(beams.keys())[0]
# number_of_beams = get_beams_recursive(beam.x, lines)
number_of_beams = sum(get_beams(beams, lines).values())

print("Number of beams:", number_of_beams)
