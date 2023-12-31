from __future__ import annotations
from typing import TypeAlias
from itertools import permutations
from abc import ABC, abstractmethod


MazeType: TypeAlias = list[str]

TILES = ".|-LJ7FS"
CARDINALITY = {
    "north": (0, -1),
    "east": (1, 0),
    "west": (-1, 0),
    "south": (0, 1),
}


class Maze:
    def __init__(self, maze: MazeType) -> None:
        self.maze = maze
        self.width = len(maze[0])-1
        self.height = len(maze)-1

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Maze):
            return False
        return self.maze == other.maze

    def __getitem__(self, key: Position) -> str:
        if key.maze != self:
            raise ValueError("Maze of position provided is different.")

        x, y = key.x, key.y

        return self.maze[y][x]

    def find_start(self) -> Tile:
        for i, line in enumerate(self.maze):
            for j, char in enumerate(line):
                if char == "S":
                    break
            else:
                continue
            break
        else:
            i, j = -1, -1

        return Tile(self, j, i)

    def __iter__(self):
        return MazeIterator(self)


class MazeIterator:
    def __init__(self, maze: Maze) -> None:
        self.maze = maze
        self.i = 0
        self.j = 0

    def __next__(self):
        if self.j > self.maze.width:
            self.i += 1
            self.j = 0
        if self.i > self.maze.height:
            raise StopIteration

        tile = Tile(self.maze, self.j, self.i)
        self.j += 1

        return tile


class Position:
    def __init__(self, maze: Maze, x: int, y: int):
        self.maze = maze
        self.x = x
        self.y = y
        self.__validate()

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Position):
            return False
        comp1 = self.maze == other.maze
        comp2 = self.x == other.x
        comp3 = self.y == other.y

        return comp1 and comp2 and comp3

    def __hash__(self) -> int:
        maze_identifier = "".join(self.maze.maze)

        return hash((self.x, self.y, maze_identifier))

    def __str__(self) -> str:
        string = f"Position[{self.x}, {self.y}]"

        return string

    def __repr__(self) -> str:
        string = self.__str__()

        return string

    def __validate(self) -> None:
        if self.x < 0:
            self.x = 0
        elif self.x > self.maze.width:
            self.x = self.maze.width
        if self.y < 0:
            self.y = 0
        elif self.y > self.maze.height:
            self.y = self.maze.height

    def get_surroundings(self) -> set[Position]:
        positions = set()
        possible_combinations = list(permutations([0, 1]))
        possible_combinations += list(permutations([0, -1]))
        for x, y in possible_combinations:
            new_position = Position(self.maze,
                                    self.x+x,
                                    self.y+y)
            positions.add(new_position)

        positions.discard(Position(self.maze, self.x, self.y))

        return positions


class AbstractTile(ABC):
    def __init__(self, tile: str, x: int, y: int):
        if tile not in TILES:
            raise ValueError(f"Tile must be one of '{TILES}'")

        self.tile = tile
        self.__get_positions(x, y)

    def __get_positions(self, x: int, y: int) -> None:
        match self.tile:
            case ".":
                card = []
            case "S":
                card = list(CARDINALITY.keys())
            case "-":
                card = ["east", "west"]
            case "|":
                card = ["south", "north"]
            case "L":
                card = ["north", "east"]
            case "J":
                card = ["north", "west"]
            case "7":
                card = ["south", "west"]
            case "F":
                card = ["south", "east"]

        self.pipes = self.__process_positions(card, x, y)

    def __process_positions(self,
                            cardinality: list[str],
                            x: int,
                            y: int) -> set[tuple[int, int]]:
        positions = set()
        for card in cardinality:
            new_x, new_y = CARDINALITY[card]
            new_pos = (x+new_x, y+new_y)
            positions.add(new_pos)

        return positions

    @abstractmethod
    def get_valid_positions(self) -> set[Position]:
        pass


class Tile(AbstractTile, Position):
    def __init__(self, maze: Maze, x: int, y: int):

        super(AbstractTile, self).__init__(maze, x, y)
        tile = maze[Position(maze, x, y)]
        super().__init__(tile, self.x, self.y)

    @classmethod
    def from_position(cls, position: Position):

        return cls(position.maze, position.x, position.y)

    def __str__(self) -> str:
        string = f"Tile('{self.tile}', [{self.x}, {self.y}])"

        return string

    def __repr__(self) -> str:
        string = self.__str__()

        return string

    def __hash__(self) -> int:
        maze_identifier = "".join(self.maze.maze)

        return hash((self.tile, self.x, self.y, maze_identifier))

    def get_valid_positions(self) -> set[Position]:
        surroundings = self.get_surroundings()
        valid_positions = set()
        for (x, y) in self.pipes:
            pipe_position = Position(self.maze, x, y)
            if pipe_position in surroundings:
                valid_positions.add(Tile.from_position(pipe_position))

        return valid_positions


class Tunnel:
    def __init__(self, maze: Maze, starting_point: Tile) -> None:
        self.maze = maze
        self.starting_point = starting_point
        self._explore()

    def _explore(self) -> None:
        sequence = [self.starting_point]
        prev_tile = sequence[-1]
        while True:
            next_tile = sequence[-1]
            next_tiles = self._get_next_tiles(next_tile, prev_tile)

            # no more valid routes to follow
            if len(next_tiles) == 0:
                break

            chosen_tile = next_tiles.pop()
            # we get at the start of the loop
            if chosen_tile == self.starting_point:
                break

            sequence.append(chosen_tile)
            prev_tile = next_tile

        self.sequence = sequence

    def _get_next_tiles(self, tile: Tile, prev_tile: Tile) -> set[Tile]:
        pipe_positions = tile.get_valid_positions()
        next_pipes = set()
        for position in pipe_positions.copy():
            surrounding_tile = Tile.from_position(position)
            if surrounding_tile == prev_tile:
                continue
            surrounding_tile_pipes = surrounding_tile.get_valid_positions()
            if tile in surrounding_tile_pipes:
                next_pipes.add(surrounding_tile)

        return next_pipes

    def get_polar_opposite(self):
        length = len(self.sequence)
        opposite_index = length // 2
        if opposite_index % 2 == 0:
            max_distance = opposite_index
        else:
            max_distance = opposite_index + 1

        return (max_distance, self.sequence[opposite_index])

    def get_internal_tiles(self) -> set[Tile]:
        loop_tiles = set(self.sequence)
        internal_tiles = set()
        up_tiles = {"|", "L", "J"}
        down_tiles = {"|", "7", "F"}
        for j in range(self.maze.height+1):
            # for every tile that goes up/down,
            # there must be one that goes
            # down/up since it's a close loop.
            # This means that if we meet an
            # up/down tile while scrolling to
            # the right that wasn't immediately
            # preceded by a down/up tile,
            # everything that follows must be
            # internal or part of the loop, until
            # we find a down/up tile. If
            # every up/down tile has met a
            # corresponding down/up tile, then
            # as we scroll, every tile must be
            # external or part of the loop
            up_counter = 0
            down_counter = 0
            for i in range(0, self.maze.width+1):
                tile = Tile(self.maze, i, j)
                tile_in_loop = tile in loop_tiles

                # S is special because it can be a down
                # or right or both tile depending on the
                # context, so we need to check.
                if tile.tile == "S":
                    surroundings = tile.get_surroundings()
                    for position in surroundings:
                        surrounding_tile = Tile(
                            self.maze, position.x, position.y)
                        # check if below S there is an up tile.
                        # This means S is a down tile
                        if surrounding_tile.y > tile.y and \
                                surrounding_tile.tile in up_tiles and \
                                surrounding_tile in loop_tiles:
                            down_counter += 1
                        # check if above S there is a down tile.
                        # This means S is a up tile.
                        elif surrounding_tile.y < tile.y and \
                                surrounding_tile.tile in down_tiles and \
                                surrounding_tile in loop_tiles:
                            up_counter += 1
                if tile_in_loop and tile.tile in up_tiles:
                    up_counter += 1
                if tile_in_loop and tile.tile in down_tiles:
                    down_counter += 1

                # check if the tile is not part of the loop,
                # and if not every up/down tile has
                # been matched by a down/up tile already.
                # This means that every tile not part
                # of the loop is internal, until the
                # non matched tiles are matched.
                if not tile_in_loop and (up_counter % 2 == 1
                                         or down_counter % 2 == 1):
                    internal_tiles.add(tile)

        return internal_tiles
