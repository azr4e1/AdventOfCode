from collections import defaultdict, namedtuple
from itertools import combinations
from typing import TypeAlias


GALAXY = "#"
EMPTY = "."

GalaxyCombination: TypeAlias = tuple[int, int]


Position = namedtuple("Position", ["col", "row"])


# position: (y, x)
class Universe:
    def __init__(self, universe: list[str]) -> None:
        self.universe = universe
        self._find_galaxies()

    def __str__(self) -> str:
        string = "\n".join(self.universe)

        return string

    def __repr__(self) -> str:

        return self.__str__()

    def _find_empty(self) -> tuple[set[int], set[int]]:
        empty_rows = set()
        empty_cols = set()
        cols = defaultdict(set)
        for row_nr, line in enumerate(self.universe):
            row = set()
            for col_nr, el in enumerate(line):
                row.add(el)
                cols[col_nr].add(el)

            # check if row is empty
            if GALAXY not in row:
                empty_rows.add(row_nr)

        for col_nr, values in cols.items():
            if GALAXY not in values:
                empty_cols.add(col_nr)

        return empty_rows, empty_cols

    def expand(self, factor: int = 2) -> None:
        empty_rows, empty_cols = self._find_empty()

        for index, galaxy in enumerate(self.galaxies):
            greater_than_row = len(
                [row for row in empty_rows if row < galaxy.row])
            greater_than_col = len(
                [col for col in empty_cols if col < galaxy.col])
            galaxy_row = galaxy.row + greater_than_row * (factor-1)
            galaxy_col = galaxy.col + greater_than_col * (factor-1)
            expanded_galaxy = Position(galaxy_col, galaxy_row)
            self.galaxies[index] = expanded_galaxy

    def _find_galaxies(self) -> None:
        galaxies: list[Position] = []
        for row, line in enumerate(self.universe):
            for col, el in enumerate(line):
                if el == GALAXY:
                    galaxies.append(Position(col, row))

        self.galaxies = galaxies

    def _distance(self, pos1: Position, pos2: Position) -> int:
        dist_col = abs(pos1.col - pos2.col)
        dist_row = abs(pos1.row - pos2.row)

        return dist_col + dist_row

    def calculate_distances(self) -> dict[GalaxyCombination, int]:
        galaxy_combinations = list(combinations(self.galaxies, 2))
        distances: dict[GalaxyCombination, int] = {}
        for comb in galaxy_combinations:
            galaxy1, galaxy2 = comb
            index1 = self.galaxies.index(galaxy1)
            index2 = self.galaxies.index(galaxy2)
            distance = self._distance(galaxy1, galaxy2)
            distances[(index1, index2)] = distance

        return distances
