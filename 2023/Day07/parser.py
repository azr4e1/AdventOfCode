from cards import Hand, make_hand


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
