from dataclasses import dataclass, field
from collections import Counter


CARD_RANKS = "2 3 4 5 6 7 8 9 T J Q K A".split()
HAND_RANKS = [
    {1: 5},  # high card
    {2: 1, 1: 3},  # one pair
    {2: 2, 1: 1},  # two pairs
    {3: 1, 1: 2},  # three of a kind
    {3: 1, 2: 1},  # full house
    {4: 1, 1: 1},  # four of a kind
    {5: 1},  # five of a kind
]
HAND_TYPES = {
    0: "high card",
    1: "one pair",
    2: "two pairs",
    3: "three of a kind",
    4: "full house",
    5: "four of a kind",
    6: "five of a kind"
}


@dataclass(order=True, unsafe_hash=True)
class Card:
    strength: int = field(init=False, repr=False)
    label: str

    def __post_init__(self) -> None:
        self.strength = CARD_RANKS.index(self.label)


@dataclass(order=True)
class Hand:
    hand_value: int = field(init=False, repr=False)
    card1: Card
    card2: Card
    card3: Card
    card4: Card
    card5: Card
    hand_type: str = field(init=False)

    def __post_init__(self) -> None:
        counted_cards = Counter([self.card1,
                                 self.card2,
                                 self.card3,
                                 self.card4,
                                 self.card5])
        counts = Counter(counted_cards.values())
        for index, el in enumerate(HAND_RANKS):
            if el == counts:
                self.hand_value = index
                self.hand_type = HAND_TYPES[index]
                break


def make_hand(cards: str) -> Hand:
    if len(cards) != 5:
        raise ValueError
    cards_obj = []
    for card in cards:
        if card not in CARD_RANKS:
            raise ValueError
        cards_obj.append(Card(card))

    return Hand(*cards_obj)
