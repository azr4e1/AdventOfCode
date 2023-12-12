from itertools import pairwise


class OASISequence:
    def __init__(self, sequence: list[int]) -> None:
        self.sequence = sequence

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, OASISequence):
            return False
        return self.sequence == other.sequence

    def next(self) -> int:
        cls = self.__class__
        if set(self.sequence) == {0}:
            return 0
        new_seq = cls([b-a for a, b in pairwise(self.sequence)])
        increment = new_seq.next()
        next_el = self.sequence[-1] + increment

        return next_el

    def prev(self) -> int:
        cls = self.__class__
        if set(self.sequence) == {0}:
            return 0
        new_seq = cls([b-a for a, b in pairwise(self.sequence)])
        decrement = new_seq.prev()
        prev_el = self.sequence[0] - decrement

        return prev_el
