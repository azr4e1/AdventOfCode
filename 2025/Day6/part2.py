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
    lines = [l for l in input.split("\n") if l.strip() != ""]
    symbols = lines[-1]
    values = lines[:-1]
    length = len(values[0])-1
    numbers = []
    block = []
    while length >= 0:
        num = ""
        for line in values:
            num += line[length]
        length -= 1
        if num.strip() == "":
            numbers.append(block)
            block = []
            continue
        block.append(int(num))

    if block:
        numbers.append(block)

    symbols = [c for c in lines[-1].split() if c != " "]
    return numbers, symbols


with open("./input.txt") as f:
    content = f.read()

numbers, symbols = parse(content)

total = 0
for i in range(len(symbols)):
    number = numbers[len(symbols) - i - 1]
    symbol = symbols[i]
    total += MAP_SYMBOL[symbol](number)

print("Result is:", total)
