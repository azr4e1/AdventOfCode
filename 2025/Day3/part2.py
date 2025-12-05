class BatteryBank:
    def __init__(self, bank: str):
        self.bank = bank

    def max_joltage(self):
        batteries_on = []
        no_batteries = 12
        for i in range(no_batteries):
            if len(batteries_on) == 0:
                start = 0
            else:
                start = batteries_on[-1] + 1
            highest_val = 0
            highest_index = 0
            end = len(self.bank) + i - (no_batteries) + 1

            for k, j in enumerate(self.bank[start:end]):
                val = int(j)
                if val > highest_val:
                    highest_val = val
                    highest_index = k
            batteries_on.append(start + highest_index)

        joltage = sum(int(self.bank[x]) * (10**(11-i))
                      for i, x in enumerate(batteries_on))

        print(batteries_on, joltage)
        return joltage


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
