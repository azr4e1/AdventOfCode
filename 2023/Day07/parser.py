from cards import Hand, make_hand
from alternative_cards import JokerHand, make_joker_hand


class Parser:
    def __init__(self, game: list[str]) -> None:
        self.game = game

    def parse(self) -> list[tuple[Hand, int]]:
        parsed: list[tuple[Hand, int]] = []
        for line in self.game:
            tmp_hand, tmp_rank = line.split()

            hand = make_hand(tmp_hand)
            rank = int(tmp_rank)
            parsed.append((hand, rank))

        return parsed


class JokerParser:
    def __init__(self, game: list[str]) -> None:
        self.game = game

    def parse(self) -> list[tuple[JokerHand, int]]:
        parsed: list[tuple[Hand, int]] = []
        for line in self.game:
            tmp_hand, tmp_rank = line.split()

            hand = make_joker_hand(tmp_hand)
            rank = int(tmp_rank)
            parsed.append((hand, rank))

        return parsed
