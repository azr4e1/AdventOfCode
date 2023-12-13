from __future__ import annotations
from typing import TypeAlias
from itertools import product
from abc import ABC, abstractmethod

MazeType: TypeAlias = list[str]

TILES = ".|-LJ7FS"
CARDINALITY = {
    "north": (0, 1),
    "east": (1, 0),
    "west": (-1, 0),
    "south": (0, -1),
    "north-east": (1, 1),
    "north-west": (-1, 1),
    "south-east": (1, -1),
    "south-west": (-1, -1)
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

        return Tile(self, "S", j, i)


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
        possible_combinations = product([-1, 0, 1], repeat=2)
        for x, y in possible_combinations:
            new_position = Position(self.maze,
                                    self.x+x,
                                    self.y+y)
            positions.add(new_position)

        positions.discard(self)

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
                return set()
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
    def __init__(self, maze: Maze, tile: str, x: int, y: int):

        super().__init__(tile, x, y)
        super(AbstractTile, self).__init__(maze, x, y)

    def __str__(self) -> str:
        string = f"Tile('{self.tile}', [{self.x}, {self.y}])"

        return string

    def __repr__(self) -> str:
        string = self.__str__()

        return string

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Tile):
            return False

        comp1 = super(AbstractTile, self).__eq__(other)
        comp2 = self.tile == other.tile

        return comp1 and comp2

    def get_valid_positions(self) -> set[Position]:
        surroundings = self.get_surroundings()
        valid_positions = set()
        for (x, y) in self.pipes:
            pipe_position = Position(self.maze, x, y)
            if pipe_position in surroundings:
                valid_positions.add(pipe_position)

        return valid_positions


class Tunnel:
    pass
