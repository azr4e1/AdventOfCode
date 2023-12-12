from sequences import OASISequence


class Parser:
    def __init__(self, sequences: list[str]):
        self.sequences = sequences

    def parse(self) -> list[OASISequence]:
        sequences = []
        for line in self.sequences:
            seq = [int(el) for el in line.split(" ")]
            sequences.append(OASISequence(seq))

        return sequences
