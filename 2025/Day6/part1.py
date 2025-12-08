def sum(mylist):
    val = 0
    for i in mylist:
        val += i
    return val


def prod(mylist):
    val = 1
    for i in mylist:
        val *= i
    return val


MAP_SYMBOL = {"*": prod, "+": sum}


def parse(input: str):
    values = []
    for line in input.split("\n"):
        if line.strip() == "":
            continue
        row = []
        for val in line.split(" "):
            val_strip = val.strip()
            if len(val_strip) == 0:
                continue
            row.append(val_strip)
        values.append(row)

    numbers = values[:-1]
    transposed = [[] for i in numbers[0]]
    for line in numbers:
        for index, i in enumerate(line):
            transposed[index].append(int(i))

    symbols = values[-1]

    return transposed, symbols


with open("./input.txt") as f:
    content = f.read()

numbers, symbols = parse(content)

total = 0
for number, symbol in zip(numbers, symbols):
    total += MAP_SYMBOL[symbol](number)

print("Result is:", total)
