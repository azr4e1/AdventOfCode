# You take the boat and find the gardener right where you were told he would
# be: managing a giant "garden" that looks more to you like a farm.

# "A water source? Island Island is the water source!" You point out that
# Snow Island isn't receiving any water.

# "Oh, we had to stop the water because we ran out of sand to filter it
# with! Can't make snow with dirty water. Don't worry, I'm sure we'll get
# more sand soon; we only turned off the water a few days... weeks... oh
# no." His face sinks into a look of horrified realization.

# "I've been so busy making sure everyone here has food that I completely
# forgot to check why we stopped getting more sand! There's a ferry leaving
# soon that is headed over in that direction - it's much faster than your
# boat. Could you please go check it out?"

# You barely have time to agree to this request when he brings up another.
# "While you wait for the ferry, maybe you can help us with our food
# production problem. The latest Island Island Almanac just arrived and
# we're having trouble making sense of it."

# The almanac (your puzzle input) lists all of the seeds that need to be
# planted. It also lists what type of soil to use with each kind of seed,
# what type of fertilizer to use with each kind of soil, what type of water
# to use with each kind of fertilizer, and so on. Every type of seed, soil,
# fertilizer and so on is identified with a number, but numbers are reused
# by each category - that is, soil 123 and fertilizer 123 aren't necessarily
# related to each other.

# For example:

# seeds: 79 14 55 13

# seed-to-soil map:
# 50 98 2
# 52 50 48

# soil-to-fertilizer map:
# 0 15 37
# 37 52 2
# 39 0 15

# fertilizer-to-water map:
# 49 53 8
# 0 11 42
# 42 0 7
# 57 7 4

# water-to-light map:
# 88 18 7
# 18 25 70

# light-to-temperature map:
# 45 77 23
# 81 45 19
# 68 64 13

# temperature-to-humidity map:
# 0 69 1
# 1 0 69

# humidity-to-location map:
# 60 56 37
# 56 93 4
# The almanac starts by listing which seeds need to be planted: seeds 79,
# 14, 55, and 13.

# The rest of the almanac contains a list of maps which describe how to
# convert numbers from a source category into numbers in a destination
# category. That is, the section that starts with seed-to-soil map:
# describes how to convert a seed number (the source) to a soil number (the
# destination). This lets the gardener and his team know which soil to use
# with which seeds, which water to use with which fertilizer, and so on.

# Rather than list every source number and its corresponding destination
# number one by one, the maps describe entire ranges of numbers that can be
# converted. Each line within a map contains three numbers: the destination
# range start, the source range start, and the range length.

# Consider again the example seed-to-soil map:

# 50 98 2
# 52 50 48
# The first line has a destination range start of 50, a source range start
# of 98, and a range length of 2. This line means that the source range
# starts at 98 and contains two values: 98 and 99. The destination range is
# the same length, but it starts at 50, so its two values are 50 and 51.
# With this information, you know that seed number 98 corresponds to soil
# number 50 and that seed number 99 corresponds to soil number 51.

# The second line means that the source range starts at 50 and contains 48
# values: 50, 51, ..., 96, 97. This corresponds to a destination range
# starting at 52 and also containing 48 values: 52, 53, ..., 98, 99. So,
# seed number 53 corresponds to soil number 55.

# Any source numbers that aren't mapped correspond to the same destination
# number. So, seed number 10 corresponds to soil number 10.

# So, the entire list of seed numbers and their corresponding soil numbers
# looks like this:

# seed  soil
# 0     0
# 1     1
# ...   ...
# 48    48
# 49    49
# 50    52
# 51    53
# ...   ...
# 96    98
# 97    99
# 98    50
# 99    51
# With this map, you can look up the soil number required for each initial
# seed number:

# Seed number 79 corresponds to soil number 81.
# Seed number 14 corresponds to soil number 14.
# Seed number 55 corresponds to soil number 57.
# Seed number 13 corresponds to soil number 13.
# The gardener and his team want to get started as soon as possible, so
# they'd like to know the closest location that needs a seed. Using these
# maps, find the lowest location number that corresponds to any of the
# initial seeds. To do this, you'll need to convert each seed number through
# other categories until you can find its corresponding location number. In
# this example, the corresponding types are:

# Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78,
# humidity 78, location 82.
# Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42,
# humidity 43, location 43.
# Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82,
# humidity 82, location 86.
# Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34,
# humidity 35, location 35.
# So, the lowest location number in this example is 35.

# What is the lowest location number that corresponds to any of the initial
# seed numbers?
from __future__ import annotations
from typing import TypeAlias

Almanac: TypeAlias = dict[tuple[int, int], tuple[int, int]]


# Puzzle One
class AlmanacParser:
    HEADER_SYMBOL = " map:"

    def __init__(self, almanac: list[str]) -> None:
        self.almanac = almanac
        self.seeds = [int(seed) for seed in almanac[0].split(':')[-1].split()]
        self._get_maps()

    def _lookup(self, nr: int, map: Almanac) -> int:
        for start, end in map.keys():
            if nr >= start and nr <= end:
                distance = nr - start
                dest_start, _ = map[(start, end)]
                return dest_start + distance
        return nr

    def _get_maps(self) -> None:
        maps = {}
        ordered_maps_list = []
        current_context = ""
        context_map = {}
        for line in self.almanac[1:]:
            if not line:
                continue
            if self.HEADER_SYMBOL in line:
                if current_context:
                    maps[current_context] = context_map
                    ordered_maps_list.append(current_context)
                current_context = line.removesuffix(self.HEADER_SYMBOL)
                context_map = {}
                continue
            dest_start, source_start, nr = tuple([int(x) for x in line.split()])
            context_map[(source_start, source_start+nr)] = (dest_start, dest_start+nr)

        # wrap up
        maps[current_context] = context_map
        ordered_maps_list.append(current_context)

        self.maps = maps
        self.ordered_maps_list = ordered_maps_list

    def get_locations(self) -> list[int]:
        locations = []
        for seed in self.seeds:
            intermediate_map = seed
            for key in self.ordered_maps_list:
                map = self.maps[key]
                intermediate_map = self._lookup(intermediate_map, map)
                if "to-location" in key:
                    break
            locations.append(intermediate_map)

        return locations

# Everyone will starve if you only plant such a small number of seeds.
# Re-reading the almanac, it looks like the seeds: line actually describes
# ranges of seed numbers.

# The values on the initial seeds: line come in pairs. Within each pair, the
# first value is the start of the range and the second value is the length of
# the range. So, in the first line of the example above:

# seeds: 79 14 55 13
# This line describes two ranges of seed numbers to be planted in the garden.
# The first range starts with seed number 79 and contains 14 values: 79, 80,
# ..., 91, 92. The second range starts with seed number 55 and contains 13
# values: 55, 56, ..., 66, 67.

# Now, rather than considering four seed numbers, you need to consider a total
# of 27 seed numbers.

# In the above example, the lowest location number can be obtained from seed
# number 82, which corresponds to soil 84, fertilizer 84, water 84, light 77,
# temperature 45, humidity 46, and location 46. So, the lowest location number
# is 46.

# Consider all of the initial seed numbers listed in the ranges on the first
# line of the almanac. What is the lowest location number that corresponds to
# any of the initial seed numbers?


# Second Puzzle
class Range:
    def __init__(self, start: int, length: int) -> None:
        if length <= 0:
            raise ValueError("Length must be greater than 0")
        if not isinstance(start, int):
            raise ValueError("Start must be an integer")
        if not isinstance(length, int):
            raise ValueError("Length must be an integer")
        self.start = start
        self.end = start + length - 1
        self.length = length

    def __str__(self) -> str:
        return f"Range[{self.start}, {self.end}]"

    def __repr__(self) -> str:
        return f"Range[{self.start}, {self.end}]"

    def __eq__(self, other: Range) -> bool:
        comp1 = self.start == other.start
        comp2 = self.end == other.end

        return comp1 and comp2

    def __hash__(self) -> int:
        return hash((self.start, self.length))

    def __add__(self, other: Range) -> MultiRange:
        if other.start-self.end > 1 or self.start-other.end>1:
            return MultiRange(self, other)
        else:
            cls = self.__class__
            extremes = {self.start, self.end, other.start, other.end}
            start = min(extremes)
            length = max(extremes) - start + 1
            new_range = cls(start, length)

            return MultiRange(new_range)

    def __sub__(self, other: Range) -> MultiRange:
        cls = self.__class__
        if self.end < other.start or self.start > other.end:
            return MultiRange(self)
        elif other.start >= self.start and other.end <= self.end:
            length1 = other.start - self.start + 1
            length2 = self.end - other.end + 1
            return MultiRange(cls(self.start, length1), cls(other.end, length2))
        elif other.start >= self.start and other.start <= self.end:
            length = other.start - self.start + 1
            return MultiRange(cls(self.start, length))
        elif other.end >= self.start and other.end <= self.end:
            length = self.end - other.end + 1
            return MultiRange(cls(other.end, length))
        else:
            return MultiRange()

    def __truediv__(self, other: Range) -> list[Range]:
        cls = self.__class__
        if self.end < other.start or self.start > other.end:
            return MultiRange()
        elif other.start >= self.start and other.end <= self.end:
            return MultiRange(other)
        elif other.start >= self.start and other.start <= self.end:
            length = self.end - other.start + 1
            return MultiRange(cls(other.start, length))
        elif other.end >= self.start and other.end <= self.end:
            length = other.end - self.start + 1
            return MultiRange(cls(self.start, length))
        else:
            return MultiRange(self)


class MultiRange(Range):
    def __init__(self, *args: Range):
        self.ranges = self._compact_ranges(args)

    def _compact_ranges(self, input: list) -> list:
        ranges = []
        for el in input:
            if not isinstance(el, Range):
                continue
            elif isinstance(el, MultiRange):
                ranges.extend(el.ranges)
            else:
                ranges.append(el)
        ranges.sort(key=lambda x: x.start)

        return ranges

    def __eq__(self, other: MultiRange) -> bool:
        if not isinstance(other, MultiRange):
            return False
        if len(self) != len(other):
            return False
        comps: list[bool] = []
        for el1, el2 in zip(self.ranges, other.ranges):
            comps.append(el1 == el2)

        return all(comps)

    def __len__(self) -> int:
        return len(self.ranges)

    def __add__(self, other: Range) -> MultiRange:
        cls = self.__class__
        return cls(self, other)

    def __sub__(self, other: Range) -> MultiRange:
        pass

    def __truediv__(self, other: Range) -> MultiRange:
        pass

    def __str__(self) -> str:
        if len(self.ranges) == 0:
            return "MultiRange[empty]"
        range_string = ", ".join([str(r) for r in self.ranges])
        return f"MultiRange[{range_string}]"

    def __repr__(self) -> str:
        return self.__str__()


class BetterAlmanacParser:
    HEADER_SYMBOL = " map:"

    def __init__(self, almanac: str) -> None:
        self.almanac = almanac
        self.original_seeds = [int(seed)
                               for seed in almanac[0].split(':')[-1].split()]
        self._get_maps()
        self.seeds = []
        for index in range(0, len(self.original_seeds)-1, 2):
            seed_nr = self.original_seeds[index]
            seed_len = self.original_seeds[index+1]
            seed_range = Range(seed_nr, seed_len)
            self.seeds.append(seed_range)
        self._get_maps()

    def _get_maps(self) -> None:
        maps = {}
        ordered_maps_list = []
        current_context = ""
        context_map = {}
        for line in self.almanac[1:]:
            if not line:
                continue
            if self.HEADER_SYMBOL in line:
                if current_context:
                    maps[current_context] = context_map
                    ordered_maps_list.append(current_context)
                current_context = line.removesuffix(self.HEADER_SYMBOL)
                context_map = {}
                continue
            dest_start, source_start, nr = tuple([int(x) for x in line.split()])
            context_map[Range(source_start, nr)] = Range(dest_start, nr)

        # wrap up
        maps[current_context] = context_map
        ordered_maps_list.append(current_context)

        self.maps = maps
        self.ordered_maps_list = ordered_maps_list

    def _map_range(self,
                   range_to_apply: Range,
                   key: Range,
                   value: Range) -> tuple[Range, list[Range]]:
        intersection = (range_to_apply / key)[0]
        if intersection:
            remaining: Range = range_to_apply - key
            start_diff = intersection.start - key.start
            mapped_range = Range(value.start+start_diff, intersection.length)
        else:
            remaining = []
            mapped_range = None

        return mapped_range, remaining

    def apply_map(self):
        translation = {}
        map_to_translate = self.seeds
        for map_name in self.ordered_maps_list:
            remaining_els = []
            mapped_els = []
            for el in map_to_translate:
                for key, value in self.maps[map_name]:
                    mapped_range, remaining = self._map_range(el,
                                                              key,
                                                              value)
                    if mapped_range is not None:
                        mapped_els.append(mapped_range)
                    remaining_els.extend(remaining)


if __name__ == "__main__":
    with open("./input.txt") as f:
        almanac = [line.strip() for line in f.readlines()]

    parser = AlmanacParser(almanac)
    print("The lowest location number is:", min(parser.get_locations()))

    better_parser = BetterAlmanacParser(almanac)
    print(better_parser.maps['humidity-to-location'])
