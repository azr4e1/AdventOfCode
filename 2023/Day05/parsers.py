from __future__ import annotations
from typing import TypeAlias, Optional
from ranges import Range, MultiRange

Almanac: TypeAlias = dict[tuple[int, int], tuple[int, int]]


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
        context_map: dict[tuple[int, int], tuple[int, int]] = {}
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


class BetterAlmanacParser:
    HEADER_SYMBOL = " map:"

    def __init__(self, almanac: list[str]) -> None:
        self.almanac = almanac
        self.original_seeds = [int(seed)
                               for seed in almanac[0].split(':')[-1].split()]
        seeds_list = []
        for index in range(0, len(self.original_seeds)-1, 2):
            seed_nr = self.original_seeds[index]
            seed_len = self.original_seeds[index+1]
            seed_range = Range(seed_nr, seed_len)
            seeds_list.append(seed_range)
        self.seeds = MultiRange(*seeds_list)
        self._get_maps()

    def _get_maps(self) -> None:
        maps = {}
        ordered_maps_list = []
        current_context = ""
        context_map: dict[Range, Range] = {}
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
                   value: Range) -> Optional[Range]:
        intersection = (range_to_apply / key)
        if intersection != MultiRange():
            common_el = intersection.ranges[0]
            start_diff = common_el.start - key.start
            mapped_range = Range(value.start+start_diff, common_el.length)
        else:
            mapped_range = None

        return mapped_range

    def apply_map(self):
        map_to_translate = self.seeds
        for map_name in self.ordered_maps_list:
            keys = MultiRange(*self.maps[map_name].keys())
            intersection = map_to_translate / keys
            map_to_translate = map_to_translate - keys
            mapped_ranges_list = []
            for el in intersection.ranges:
                for key, value in self.maps[map_name].items():
                    mapped_range = self._map_range(el, key, value)
                    if mapped_range is not None:
                        mapped_ranges_list.append(mapped_range)
            mapped_ranges = MultiRange(*mapped_ranges_list)
            map_to_translate += mapped_ranges
        return map_to_translate
