class BatteryBank:
    def __init__(self, bank: str):
        self.bank = bank

    def max_joltage(self):
        highest = 0
        second_highest = 0
        for j, i in enumerate(self.bank):
            val = int(i.strip())
            if val > highest and j < len(self.bank)-1:
                highest = val
                second_highest = 0
                continue
            if val > second_highest:
                second_highest = val

        return highest * 10 + second_highest


def parse(file: str) -> list[BatteryBank]:
    banks = []
    with open(file) as f:
        for line in f:
            banks.append(BatteryBank(line.strip()))

    return banks


banks = parse("./input.txt")
max_jolts = []
for bank in banks:
    max_jolts.append(bank.max_joltage())
print("Max Joltage is:", sum(max_jolts))
