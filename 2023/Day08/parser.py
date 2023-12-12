class Parser:
    def __init__(self, map: list[str]):
        self.map = map
        self.parse()

    def parse(self) -> None:
        self.instructions = list(self.map[0])
        network = {}
        for line in self.map[2:]:
            node, directions = line.split(" = ")
            left_dir, right_dir = directions.split(", ")
            network[node] = {
                "L": left_dir[1:],
                "R": right_dir[:-1]
            }
        self.network = network
