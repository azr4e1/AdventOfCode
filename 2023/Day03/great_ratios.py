# --- Day 3: Gear Ratios ---

# You and the Elf eventually reach a gondola lift station; he says the
# gondola lift will take you up to the water source, but this is as far as
# he can bring you. You go inside.

# It doesn't take long to find the gondolas, but there seems to be a
# problem: they're not moving.

# "Aaah!"

# You turn around to see a slightly-greasy Elf with a wrench and a look of
# surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't
# working right now; it'll still be a while before I can fix it." You offer
# to help.

# The engineer explains that an engine part seems to be missing from the
# engine, but nobody can figure out which one. If you can add up all the
# part numbers in the engine schematic, it should be easy to work out which
# part is missing.

# The engine schematic (your puzzle input) consists of a visual
# representation of the engine. There are lots of numbers and symbols you
# don't really understand, but apparently any number adjacent to a symbol,
# even diagonally, is a "part number" and should be included in your sum.
# (Periods (.) do not count as a symbol.)

# Here is an example engine schematic:

# 467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598..

# In this schematic, two numbers are not part numbers because they are not
# adjacent to a symbol: 114 (top right) and 58 (middle right). Every other
# number is adjacent to a symbol and so is a part number; their sum is 4361.

# Of course, the actual engine schematic is much larger. What is the sum of
# all of the part numbers in the engine schematic?

# First Puzzle

from typing import TypeAlias
from string import punctuation
from itertools import product
from enum import Enum
from dataclasses import dataclass

Point2D: TypeAlias = tuple[int, int]
Schematics: TypeAlias = list[str]

ENGINE_SYMBOLS = punctuation.replace('.', '')


class Number(Enum):
    OUT = 0
    IN = 1


@dataclass
class EnginePart:
    value: int
    is_engine_part: bool
    positions: list[Point2D]


class SchematicsParser:
    def __init__(self, schematic: Schematics):
        self.schematic = schematic
        self._parse()

    def get_parts(self) -> list[EnginePart]:
        return self._engine_parts

    def get_symbols(self) -> list[tuple[str, Point2D]]:
        return self._symbols

    def _get_surrounding(self, position: Point2D) -> list[Point2D]:
        (x, y) = position
        x_length = len(self.schematic[0])-1
        y_length = len(self.schematic)-1
        surrounding = list(product([x-1, x, x+1],
                                   [y-1, y, y+1]))
        surrounding.remove((x, y))
        possible_surrounding: list[Point2D] = []
        for (x, y) in surrounding:
            if x < 0 or y < 0 or x > x_length or y > y_length:
                continue
            possible_surrounding.append((x, y))

        return possible_surrounding

    def _is_engine_part(self, position: Point2D) -> bool:
        possible_surroundings = self._get_surrounding(position)
        for (x, y) in possible_surroundings:
            el = self.schematic[y][x]
            if el in ENGINE_SYMBOLS:
                return True

        return False

    def _parse(self) -> None:
        engine_parts: list[EnginePart] = []
        symbols: list[tuple[str, Point2D]] = []
        for y, line in enumerate(self.schematic):
            context = Number.OUT
            for x, c in enumerate(line):
                if c.isdigit() and context is Number.OUT:
                    context = Number.IN
                    value = c
                    positions = [(x, y)]
                    is_engine_part = [self._is_engine_part(position=(x, y))]
                elif c.isdigit() and context is Number.IN:
                    value += c
                    positions.append((x, y))
                    is_engine_part.append(self._is_engine_part((x, y)))
                elif not c.isdigit() and context is Number.IN:
                    context = Number.OUT

                    int_value = int(value)
                    engine_part = EnginePart(int_value,
                                             any(is_engine_part),
                                             positions)
                    engine_parts.append(engine_part)

                    value = ''
                    positions: list[Point2D] = []
                    is_engine_part = False

                if c in ENGINE_SYMBOLS:
                    symbols.append((c, (x, y)))

        self._engine_parts = engine_parts
        self._symbols = symbols


# The engineer finds the missing part and installs it in the engine! As the
# engine springs to life, you jump in the closest gondola, finally ready to
# ascend to the water source.

# You don't seem to be going very fast, though. Maybe something is still
# wrong? Fortunately, the gondola has a phone labeled "help", so you pick it
# up and the engineer answers.

# Before you can explain the situation, she suggests that you look out the
# window. There stands the engineer, holding a phone in one hand and waving
# with the other. You're going so slowly that you haven't even left the
# station. You exit the gondola.

# The missing part wasn't the only issue - one of the gears in the engine is
# wrong. A gear is any * symbol that is adjacent to exactly two part
# numbers. Its gear ratio is the result of multiplying those two numbers
# together.

# This time, you need to find the gear ratio of every gear and add them all
# up so that the engineer can figure out which gear needs to be replaced.

# Consider the same engine schematic again:

# 467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598..

# In this schematic, there are two gears. The first is in the top left; it
# has part numbers 467 and 35, so its gear ratio is 16345. The second gear
# is in the lower right; its gear ratio is 451490. (The * adjacent to 617 is
# not a gear because it is only adjacent to one part number.) Adding up all
# of the gear ratios produces 467835.


class AdvancedSchematicsParser(SchematicsParser):
    def find_gears(self) -> dict[Point2D, int]:
        gears = {}
        for (sym, position) in self._symbols:
            if sym != '*':
                continue
            engine_parts = []
            surrounding = self._get_surrounding(position)
            for engine in self._engine_parts:
                for pos in surrounding:
                    if pos in engine.positions:
                        engine_parts.append(engine.value)
                        break
            if len(engine_parts) == 2:
                gears[position] = engine_parts[0] * engine_parts[1]

        return gears


if __name__ == "__main__":
    schematics = []
    with open("./input.txt") as f:
        for line in f:
            schematics.append(line)

    schematics_read = SchematicsParser(schematics)
    all_numbers = schematics_read.get_parts()
    engine_parts = [engine.value for engine in all_numbers
                    if engine.is_engine_part]
    print("The sum of all the part numbers in the engine schematic is:",
          sum(engine_parts))

    advanced_schematics = AdvancedSchematicsParser(schematics)
    gears = advanced_schematics.find_gears()
    print("The sum of all of the gear ratios in your engine schematic is:",
          sum(gears.values()))
