from __future__ import annotations
from sys import maxsize


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

    def __eq__(self, other) -> bool:
        if not isinstance(other, Range):
            return False
        comp1 = self.start == other.start
        comp2 = self.end == other.end

        return comp1 and comp2

    def __hash__(self) -> int:
        return hash((self.start, self.length))

    def __add__(self, other: Range) -> MultiRange:
        if other.start-self.end > 1 or self.start-other.end > 1:
            return MultiRange(self, other)
        else:
            cls = self.__class__
            extremes = {self.start, self.end, other.start, other.end}
            start = min(extremes)
            length = max(extremes) - start + 1
            new_range = cls(start, length)

            return MultiRange(new_range)

    def __truediv__(self, other: Range) -> MultiRange:
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

    def complement(self) -> MultiRange:
        # get complement
        minsize = -1*maxsize
        complement_range1 = Range(minsize, self.start - minsize)
        complement_range2 = Range(self.end+1, maxsize - self.end)
        complement = MultiRange(complement_range1, complement_range2)

        return complement

    def __sub__(self, other: Range) -> MultiRange:
        complement = other.complement()

        return complement / self


class MultiRange(Range):
    def __init__(self, *args: Range):
        self.ranges = self._compact_ranges(args)

    def _compact_ranges(self, input: tuple[Range, ...]) -> list[Range]:
        ranges = []
        for el in input:
            if not isinstance(el, Range):
                continue
            elif isinstance(el, MultiRange):
                ranges.extend(el.ranges)
            else:
                ranges.append(el)
        ranges.sort(key=lambda x: x.start)

        compacted_ranges: list[Range] = []
        for range in ranges:
            for index, old_range in enumerate(compacted_ranges):
                if old_range / range != MultiRange() \
                        or range.start-old_range.end == 1 \
                        or old_range.start-range.end == 1:
                    del compacted_ranges[index]
                    compacted_ranges.append((old_range+range).ranges[0])
                    break
            else:
                compacted_ranges.append(range)

        # compacted_ranges.sort(key=lambda x: x.start)
        return compacted_ranges

    def __eq__(self, other) -> bool:
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

    def __truediv__(self, other: Range) -> MultiRange:
        new_ranges: list[Range] = []
        cls = self.__class__
        for range in self.ranges:
            if not isinstance(other, MultiRange):
                result = range / other
                new_ranges.append(result)
            else:
                for other_range in other.ranges:
                    result = range / other_range
                    new_ranges.append(result)

        return cls(*new_ranges)

    def complement(self):
        if len(self) == 0:
            return MultiRange(Range(-1*maxsize, maxsize))
        elif len(self) == 1:
            return self.ranges[0].complement()
        else:
            complements = MultiRange(self.ranges[0].complement())
            for range in self.ranges[1:]:
                compl = range.complement()
                complements /= compl

            return complements

    def __sub__(self, other: Range) -> MultiRange:
        new_ranges: list[Range] = []
        cls = self.__class__
        for range in self.ranges:
            if not isinstance(other, MultiRange):
                result = range - other
                new_ranges.append(result)
            else:
                for other_range in other.complement().ranges:
                    complement = other_range
                    result = complement / range
                    new_ranges.append(result)

        return cls(*new_ranges)

    def __str__(self) -> str:
        if len(self.ranges) == 0:
            return "MultiRange[empty]"
        range_string = ", ".join([str(r) for r in self.ranges])
        return f"MultiRange[{range_string}]"

    def __repr__(self) -> str:
        return self.__str__()
