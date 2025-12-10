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
    for beam in list(beams):
        beams.remove(beam)
        beam.y += 1
        beams.add(beam)


def split_beam(beam: Position):
    return Position(beam.x-1, beam.y), Position(beam.x+1, beam.y)


def parse(input: str):
    beams = set()
    lines = input.split("\n")
    initial_line = lines[0]
    beams.add(Position(initial_line.index("S"), 0))

    return beams.copy(), lines


def get_beams(beams, diagram):
    split_times = 0
    for y, line in enumerate(diagram):
        for x, el in enumerate(line):
            if el == "^":
                splitter = Position(x, y)
                if splitter in beams:
                    split_times += 1
                    beams.remove(splitter)
                    beam1, beam2 = split_beam(splitter)
                    beams.add(beam1)
                    beams.add(beam2)
        fall_beams(beams)

    return beams, split_times


with open("./input.txt") as f:
    content = f.read()

beams, lines = parse(content)

beams, split_times = get_beams(beams, lines)

print("Number of beams:", split_times)
