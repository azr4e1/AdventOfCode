from dataclasses import dataclass, field
from collections import Counter


CARD_RANKS = "J 2 3 4 5 6 7 8 9 T Q K A".split()
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
class JokerCard:
    strength: int = field(init=False, repr=False)
    label: str

    def __post_init__(self) -> None:
        self.strength = CARD_RANKS.index(self.label)


@dataclass(order=True)
class JokerHand:
    hand_value: int = field(init=False, repr=False)
    card1: JokerCard
    card2: JokerCard
    card3: JokerCard
    card4: JokerCard
    card5: JokerCard
    hand_type: str = field(init=False)

    def __post_init__(self) -> None:
        counted_cards = Counter([self.card1,
                                 self.card2,
                                 self.card3,
                                 self.card4,
                                 self.card5])
        if JokerCard("J") in counted_cards.keys():
            nr_joker = counted_cards[JokerCard("J")]
            if nr_joker == 5:
                pass
            else:
                del counted_cards[JokerCard("J")]
                snd_card, snd_value = max(counted_cards.items(),
                                          key=lambda x: x[1])
                counted_cards[snd_card] = snd_value + nr_joker
        counts = Counter(counted_cards.values())
        for index, el in enumerate(HAND_RANKS):
            if el == counts:
                self.hand_value = index
                self.hand_type = HAND_TYPES[index]
                break
