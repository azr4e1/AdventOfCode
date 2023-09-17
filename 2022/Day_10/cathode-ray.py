lines = []
with open('./input.txt') as f:
    for line in f:
        lines.append(line.strip())

# === First Part ===
wait_cycle = {'noop': 1,
              'addx': 2}

cycle = 1
X = 1

cycles = {1: 1}
for line in lines:
    read = line.split(' ')
    command = read[0]
    if command == 'noop':
        cycle += wait_cycle[command]
    elif command == 'addx':
        cycles[cycle+1] = X
        value = int(read[1])
        cycle += wait_cycle[command]
        X += value

    cycles[cycle] = X

cycles_of_interest = [20, 60, 100, 140, 180, 220]
cycles_strength = {cycle: cycle*value for cycle, value in cycles.items()}
cycles_of_interest_strength = sum([cycles_strength[key] for key in cycles_of_interest])


# === Second Part ===
def draw_screen(cycles):
    for i in range(6):
        for j in range(40):
            cycle = 40*i + j+1
            pixel = j
            sprite = cycles[cycle]
            if pixel in [sprite-1, sprite, sprite+1]:
                print('#', end='')
            else:
                print('.', end='')
        print()


draw_screen(cycles)
